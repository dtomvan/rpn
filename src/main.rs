//! Maybe a blog post (like amos does) on this?
#![feature(
    is_some_with,
    drain_filter,
    maybe_uninit_uninit_array,
    byte_slice_trim_ascii
)]

mod printf;
mod stack_entry;
use anyhow::{anyhow, Result};
use chrono::{Local, Utc};
use printf::Primitive;
use rand::{
    prelude::{SliceRandom, ThreadRng},
    Rng,
};
use stack_entry::StackEntry;

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap, VecDeque},
    mem::MaybeUninit,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Sub},
    time::Instant,
};

use float_ord::FloatOrd;
use md4::Md4;
use rustyline::{Config, EditMode::Emacs};
use sha2::Sha256;

const MAX_STACK_SIZE: usize = 128;

#[derive(Debug)]
struct App<'a> {
    stack_ptr: isize,
    // Unsafety in order to represent the array of uninitialized values in
    // rpn.qc
    stack: [MaybeUninit<StackEntry<'a>>; MAX_STACK_SIZE],
    name: String,
    not_set_ptrs: VecDeque<isize>,
    start_time: Instant,
    cvars: HashMap<String, StackEntry<'a>>,
    database: VecDeque<StackEntry<'a>>,
    rng: ThreadRng,
}

impl<'a> App<'a> {
    fn new(name: String, start_time: Instant) -> Self {
        App {
            stack_ptr: -1,
            stack: MaybeUninit::uninit_array(),
            name,
            not_set_ptrs: VecDeque::new(),
            start_time,
            cvars: HashMap::new(),
            database: VecDeque::new(),
            rng: rand::thread_rng(),
        }
    }
    // TODO: Maybe make this an enum?
    fn empty_stack(&self) -> anyhow::Error {
        anyhow!("{}: empty stack", self.name)
    }
    fn stack_underflow() -> anyhow::Error {
        anyhow!("Warning: stack underflow")
    }
    fn stack_overflow() -> anyhow::Error {
        anyhow!("Warning: stack overflow")
    }
    fn push(&mut self, value: StackEntry<'a>) -> Result<()> {
        if self.stack_ptr < (MAX_STACK_SIZE - 1) as isize {
            self.stack_ptr += 1;
            self.stack[self.stack_ptr as usize] = MaybeUninit::new(value);
            Ok(())
        } else {
            Err(Self::stack_overflow())
        }
    }
    fn pop(&mut self) -> Result<StackEntry<'a>> {
        if self.stack_ptr >= 0 {
            let value = unsafe { self.stack[self.stack_ptr as usize].assume_init_read() };
            self.stack[self.stack_ptr as usize] = MaybeUninit::uninit();
            self.stack_ptr -= 1;
            Ok(value)
        } else {
            Err(Self::stack_underflow())
        }
    }
    fn pop2(&mut self) -> Result<[StackEntry<'a>; 2]> {
        self.pop().and_then(|x| self.pop().map(|y| [x, y]))
    }
    fn pop3(&mut self) -> Result<[StackEntry<'a>; 3]> {
        self.pop2().and_then(|[x, y]| self.pop().map(|z| [x, y, z]))
    }
    // used for bound command
    fn flt3(&mut self) -> Result<[f64; 3]> {
        self.pop3().and_then(|x| {
            let op: Vec<_> = x.into_iter().map(|x| x.try_as_number().ok()).collect();
            let all = op.iter().all(|x| x.is_some());
            if all {
                Ok(op
                    .into_iter()
                    .map(|x| x.unwrap())
                    .collect::<Vec<_>>()
                    .try_into()
                    .expect("Wrong array size. Bug in pop3?"))
            } else {
                Err(anyhow!("Not enough floats on the stack"))
            }
        })
    }
    fn clear(&mut self) {
        while self.stack_ptr >= 0 {
            let value = self.pop().unwrap();
            eprintln!("{}: Still on stack: {}", self.name, value);
        }
        debug_assert_eq!(self.stack_ptr, -1, "Stack has to be empty after clearing.");
    }
    fn dup(&mut self) -> Result<()> {
        if self.stack_ptr < 0 {
            return Err(Self::stack_underflow());
        }
        // SAFETY: provided that stack_ptr was managed correctly (which is done
        // in push, pop and dup), and stack_ptr was initialized as -1;
        // We can assume that the current pointer on the stack is valid (and
        // thus initialized) and we can safely reference it.
        self.push(unsafe {
            self.stack[self.stack_ptr as usize]
                .assume_init_ref()
                .clone()
        })?;
        Ok(())
    }
    fn exch(&mut self) -> Result<()> {
        if self.stack_ptr < 1 {
            return Err(Self::stack_underflow());
        }
        let first = self.pop().unwrap();
        let second = self.pop().unwrap();
        self.push(first)?;
        self.push(second)?;
        Ok(())
    }
    fn mod_1(&mut self, f: impl FnOnce(StackEntry<'a>) -> Option<StackEntry<'a>>) -> Result<()> {
        if let Some(value) = self.pop().ok().and_then(|x| f(x)) {
            Ok(self.push(value)?)
        } else {
            Err(self.empty_stack())
        }
    }
    fn nmod_1(&mut self, f: impl FnOnce(isize) -> isize) -> Result<()> {
        self.mod_1(|x| x.try_as_int().ok().map(|x| StackEntry::num(f(x) as f64)))
    }
    fn fmod_1(&mut self, f: impl FnOnce(f64) -> f64) -> Result<()> {
        self.mod_1(|x| x.try_as_number().ok().map(|x| StackEntry::num(f(x))))
    }
    fn mod_2(
        &mut self,
        f: impl FnOnce(StackEntry<'a>, StackEntry<'a>) -> Option<StackEntry<'a>>,
    ) -> Result<()> {
        if let Some(value) = self
            .pop()
            .ok()
            .and_then(|second| self.pop().ok().map(|first| (first, second)))
            .and_then(|(x, y)| f(x, y))
        {
            Ok(self.push(value)?)
        } else {
            Err(self.empty_stack())
        }
    }
    fn bmod_2(&mut self, f: impl FnOnce(bool, bool) -> bool) -> Result<()> {
        // cast bool -> int : true -> 1, false -> 2
        // cast 1 -> f64 : 1.0
        // cast 0 -> f64 : 0.0
        // thus casting bool -> int -> f64 -> 0.0 or 1.0
        self.mod_2(|x, y| Some(StackEntry::num(f(x.bool(), y.bool()) as u8 as f64)))
    }
    fn fmod_2(&mut self, f: impl FnOnce(f64, f64) -> f64) -> Result<()> {
        self.mod_2(|x, y| {
            x.try_as_number()
                .ok()
                .zip(y.try_as_number().ok())
                .map(|(x, y)| StackEntry::num(f(x, y)))
        })
    }
    fn nmod_2(&mut self, f: impl FnOnce(isize, isize) -> isize) -> Result<()> {
        self.mod_2(|x, y| {
            x.try_as_int()
                .ok()
                .zip(y.try_as_int().ok())
                .map(|(x, y)| StackEntry::num(f(x, y) as f64))
        })
    }
    fn modb_2(&mut self, f: impl FnOnce(StackEntry<'a>, StackEntry<'a>) -> bool) -> Result<()> {
        self.mod_2(|x, y| Some(StackEntry::num(if f(x, y) { 1.0 } else { 0.0 })))
    }
    fn bfmod_2(&mut self, f: impl FnOnce(f64, f64) -> bool) -> Result<()> {
        self.fmod_2(|x, y| if f(x, y) { 1.0 } else { 0.0 })
    }
    fn start_set(&mut self) {
        self.not_set_ptrs.push_front(self.stack_ptr);
    }
    fn end_set(&mut self) -> Result<()> {
        if let Some(not_set_ptr) = self.not_set_ptrs.pop_front() {
            let mut set = BTreeSet::new();
            while self.stack_ptr > not_set_ptr {
                set.insert(self.pop().unwrap());
            }
            self.push(StackEntry::Set(set))?;
            Ok(())
        } else {
            Err(self.empty_stack())
        }
    }
}

fn main() -> Result<()> {
    let start_time = Instant::now();
    let mut args = std::env::args().peekable();
    let name = args.next().unwrap();
    let prompt = format!("{name}> ");
    if let Some(string) = args.peek() {
        if string.as_str() == "--help" {
            help(&name);
            return Ok(());
        }
    }
    let mut app = App::new(name, start_time);
    let expr: Vec<_> = args.collect();
    if !expr.is_empty() {
        eval(&mut app, expr)?;
        app.clear();
        return Ok(());
    }

    let mut editor = rustyline::Editor::<()>::with_config(
        Config::builder()
            .tab_stop(4)
            .edit_mode(Emacs)
            .history_ignore_dups(true)
            .auto_add_history(true)
            .build(),
    )?;
    let path = dirs::cache_dir()
        .map(|d| d.join(".rpn_history"))
        .ok_or_else(|| String::from("Cannot find histfile."))
        .map(|path| {
            if let Err(e) = editor.load_history(&path) {
                eprintln!("Warning: no history found: {e}");
            }
            path
        })
        .map_err(|e| {
            eprintln!("Warning: no histfile found: {e}");
        });

    loop {
        let line = editor.readline(prompt.as_str())?;
        if let Err(e) = eval(&mut app, line.split_whitespace().collect()) {
            eprintln!("Warning: evaluation errored: {e}");
        }
        app.clear();
        if let Ok(ref path) = path {
            let _ = editor.save_history(&path);
        }
    }
}

#[inline]
fn return_date(app: &mut App, formatter: &mut dyn FnMut(&str) -> String) -> Result<()> {
    let format_string = app.pop()?.try_into_string()?;
    app.push(StackEntry::String(Cow::Owned(formatter(&format_string))))
}

fn eval(app: &mut App, line: Vec<impl AsRef<str>>) -> Result<()> {
    for command in line {
        match command.as_ref() {
            "pop" => {
                app.pop()?;
                Ok(())
            }
            "dup" => app.dup(),
            "exch" => app.exch(),
            "load" => {
                let cvar = app.pop().and_then(StackEntry::try_into_string)?;
                let entry = app.cvars.get(&cvar);
                if let Some(entry) = entry {
                    app.push(entry.clone())?;
                }
                Ok(())
            }
            "def" | "=" => {
                let value = app.pop()?;
                let cvar = app.pop().and_then(StackEntry::try_into_string)?;
                app.cvars.insert(cvar, value);
                Ok(())
            }
            "add" | "+" | "union" => app.mod_2(Add::add),
            "sub" | "-" | "difference" => app.mod_2(Sub::sub),
            "mul" | "*" => app.mod_2(Mul::mul),
            "div" | "/" | "intersection" => app.mod_2(Div::div),
            "mod" | "%" => app.fmod_2(|x, y| x.powf(y)),
            "and" | "&&" => app.bmod_2(|x, y| x && y),
            "or" | "||" => app.bmod_2(|x, y| x || y),
            "xor" | "^^" => app.bmod_2(|x, y| x ^ y),
            "bitand" | "&" => app.nmod_2(BitAnd::bitand),
            "bitor" | "|" => app.nmod_2(BitOr::bitor),
            "bitxor" | "^" => app.nmod_2(BitXor::bitxor),
            "eq" | "==" => app.modb_2(|x, y| x == y),
            "ne" | "!=" => app.modb_2(|x, y| x != y),
            "gt" | ">" => app.bfmod_2(|x, y| x > y),
            "ge" | ">=" => app.bfmod_2(|x, y| x >= y),
            "lt" | "<" => app.bfmod_2(|x, y| x < y),
            "le" | "<=" => app.bfmod_2(|x, y| x <= y),
            "max" => app.fmod_2(|x, y| x.max(y)),
            "min" => app.fmod_2(|x, y| x.min(y)),
            "neg" | "~" => app.fmod_1(|x| -x),
            "abs" => app.fmod_1(|x| x.abs()),
            "sgn" => app.fmod_1(|x| x.signum()),
            "rand" => {
                let max = app.pop().and_then(|x| x.try_as_int())?;
                let num = app.rng.gen_range(0..max) as f64;
                app.push(StackEntry::num(num))
            }
            "jsrand" => {
                let num = app.rng.gen();
                app.push(StackEntry::num(num))
            }
            "floor" | "f" => app.fmod_1(|x| x.floor()),
            "ceil" | "c" => app.fmod_1(|x| x.ceil()),
            "not" => app.mod_1(|x| Some(StackEntry::num(!x.bool() as u8 as f64))),
            "bitnot" => app.nmod_1(|x| !x),
            "exp" | "log" => Err(anyhow!("Unclear what these do")),
            "sin" => app.fmod_1(|x| x.sin()),
            "cos" => app.fmod_1(|x| x.cos()),
            "bound" => {
                let [high, n, low] = app.flt3()?;
                app.push(StackEntry::num(n.clamp(low, high)))
            }
            "when" => {
                let b = app.pop().map(|x| x.bool())?;
                if !b {
                    app.exch()?;
                }
                app.pop()?;
                Ok(())
            }
            // We can't really do it on a set.
            // Sets are of either unspecified or sorted order.
            // There is no such thing as an unsorted set with custom order (at least, that I know
            // of).
            "shuffle" => {
                let s = app.pop().and_then(StackEntry::try_into_string)?;
                let mut vec = Vec::from_iter(s.split(' '));
                vec.shuffle(&mut app.rng);
                app.push(StackEntry::String(Cow::Owned(vec.join(" "))))
            }
            "put" => Err(anyhow!("TODO")),
            "get" => Err(anyhow!("TODO")),
            "dbpush" => Err(anyhow!("TODO")),
            "dbpop" => Err(anyhow!("TODO")),
            "dbget" => Err(anyhow!("TODO")),
            "dblen" => Err(anyhow!("TODO")),
            "dbat" => Err(anyhow!("TODO")),
            "dbclr" => Err(anyhow!("TODO")),
            "dbsave" => Err(anyhow!("TODO")),
            "dbload" => Err(anyhow!("TODO")),
            "dbins" => Err(anyhow!("TODO")),
            "dbext" => Err(anyhow!("TODO")),
            "dbread" => Err(anyhow!("TODO")),
            "dbmov" => Err(anyhow!("TODO")),
            "dbgoto" => Err(anyhow!("TODO")),
            "localtime" => return_date(app, &mut |s| Local::now().format(s).to_string()),
            "gmtime" => return_date(app, &mut |s| Utc::now().format(s).to_string()),
            "time" => app.push(StackEntry::Number(FloatOrd(
                app.start_time.elapsed().as_secs_f64(),
            ))),
            "digest" => app.mod_2(|digest_type, data| {
                let output = match digest_type {
                    StackEntry::Sha256 => data.hash::<Sha256>(),
                    StackEntry::Md4 => data.hash::<Md4>(),
                    _ => vec![],
                };
                Some(StackEntry::String(Cow::Owned(hex::encode(output))))
            }),
            "sprintf1s" => {
                let format = app.pop().and_then(StackEntry::to_printf)?;
                let arg = app.pop()?;
                app.push(StackEntry::String(Cow::Owned(
                    format.restrict(1).format(vec![Primitive::from(arg)])?,
                )))
            }
            "printf" => {
                let format = app.pop().and_then(StackEntry::to_printf)?;
                let mut args = Vec::new();
                for _ in 0..format.args_needed() {
                    args.push(Primitive::from(app.pop()?));
                }
                app.push(StackEntry::String(Cow::Owned(format.format(args)?)))
            }
            "clear" => {
                app.stack_ptr = -1;
                Ok(())
            }
            "/MD4" => app.push(StackEntry::Md4),
            "/SHA256" => app.push(StackEntry::Sha256),
            // TODO
            "for" => Err(anyhow!("TODO")),
            "end" => Err(anyhow!("TODO")),
            s if s.chars().all(|x| x.is_ascii_digit() || x == '.') => {
                let n = s.parse()?;
                app.push(StackEntry::Number(FloatOrd(n)))?;
                Ok(())
            }
            s if s.starts_with('/') => {
                // has to be owned because of lifetime rules
                let value = s.strip_prefix('/').unwrap().to_owned();
                app.push(StackEntry::String(Cow::Owned(value)))
            }
            s if app.cvars.contains_key(s) => {
                app.push(app.cvars.get(s).unwrap().clone())
            }
            s if s.starts_with('"') && s.ends_with('"') => app.push(StackEntry::String(
                Cow::Owned(s[1..(s.len() - 1)].to_string()),
            )),
            "[" => {
                app.start_set();
                Ok(())
            }
            "]" => {
                app.end_set();
                Ok(())
            }
            _ => Err(anyhow!("{}: No such token", app.name)),
        }?;
    }
    Ok(())
}

fn help(name: &str) {
    let item_size = std::mem::size_of::<StackEntry>();
    let stack_size = item_size * MAX_STACK_SIZE;
    let stack_size_kib = stack_size.div_euclid(1024);
    // copied from http://git.xonotic.org/?p=xonotic/xonotic-data.pk3dir.git;a=blob_plain;f=qcsrc/common/command/rpn.qc
    println!("Usage: {} <expression>", name);
    println!(
        r#"    Operator description (x: string, s: set, f: float):
    x pop ----------------------------->     : removes the top
    x dup -----------------------------> x x : duplicates the top
    x x exch --------------------------> x x : swap the top two
    /cvarname load --------------------> x   : loads a cvar
    /cvarname x def ------------------->     : writes to a cvar
    f f add|sub|mul|div|mod|pow -------> f   : adds/... two numbers
    f f and|or|xor|bitand|bitor|bitxor > f   : logical and bitwise operations
    f f eq|ne|gt|ge|lt|le|max|min -----> f   : compares two numbers
    f neg|abs|sgn|rand|floor|ceil------> f   : negates/... a number
    f not|bitnot ----------------------> f   : logical and bitwise negation
    f exp|log|sin|cos -----------------> f   : exponential function & Co.
    f f f bound -----------------------> f   : bounds the middle number
    f1 f2 b when ----------------------> f   : f1 if b, f2 otherwise
    s s union|intersection|difference -> s   : set operations
    s shuffle -------------------------> s   : randomly arrange elements
    /key /value put ------------------->     : set a database key
    /key get --------------------------> s   : get a database value
    x dbpush -------------------------->     : pushes the top onto the database
    dbpop|dbget -----------------------> x   : removes/reads DB's top
    dblen|dbat ------------------------> f   : gets the DB's size/cursor pos
    dbclr ----------------------------->     : clear the DB
    s dbsave|dbload-------------------->     : save/load the DB to/from a file
    x dbins --------------------------->     : moves the top into the DB
    dbext|dbread ----------------------> x   : extract/get from the DB's cursor
    f dbmov|dbgoto -------------------->     : move or set the DB's cursor
    s localtime -----------------------> s   : formats the current local time
    s gmtime --------------------------> s   : formats the current UTC time
    time ------------------------------> f   : seconds since VM start
    s /MD4 digest ---------------------> s   : MD4 digest
    s /SHA256 digest ------------------> s   : SHA256 digest
    s /formatstring sprintf1s ---------> s   : sprintf with 1 string (pad, cut)

    Added by rust implementation:
        array literals: [ <stack operations> ]
            nested arrays are allowed
            after a closing bracket everything on the stack since the opening
            bracket will be saved
        string multiplication: s n *
            repeats the string n times like in python
        string concatenation: s s +
        time command:
            will return the time the readline impl has been running for
        jsrand command:
            will return a random real number between 0 and 1
        log|exp command:
            I have no idea what they do exactly / what log base it is
        extended stack size: 16 -> {}
            Stack item size: {} bytes
            Stack len: {} bytes
            Stack size: {} * {} = {} bytes or {} KiB
        extended printf support:
            sprintf1s still exists

            the new sprintf command works with as much stack entries as needed
            and they work on other types as well (floats, ints, chars, sets)
            a set will be joined by a space (" ") before being formatted
            the printf parser is safer than normal sprintf in c
            it will always return a string
        for command (TODO): array for <stack operations> end
            runs through every entry in the set
            for each loop, push the entry to the stack
            then do the stack operations until the end keyword is reached
            if the stack isn't empty after the operation, replace the original entry in the set
            if the stack is empty after the operation, remove the original entry in the set
        "#,
        MAX_STACK_SIZE,
        item_size,
        MAX_STACK_SIZE,
        MAX_STACK_SIZE,
        item_size,
        stack_size,
        stack_size_kib
    );
}
