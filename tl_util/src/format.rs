use std::{
    cell::{Ref, RefCell},
    collections::HashMap,
    fmt,
    sync::{Mutex, MutexGuard, RwLock, RwLockReadGuard},
};

use colored::Colorize;
use linked_hash_map::LinkedHashMap;

use crate::Rf;

#[derive(Default)]
pub enum FormatType {
    Debug,
    #[default]
    Display,
}

#[derive(Default)]
pub struct Config {
    pub format_type: FormatType,
}

impl Config {
    pub const DEBUG: Config = Config {
        format_type: FormatType::Debug,
    };

    pub const DISPLAY: Config = Config {
        format_type: FormatType::Display,
    };
}

pub enum SemanticType {
    Default,
    Type,
    Variable,
    Function,
    Keyword,
    Flow,
    String,
    Literal,
    Module,
}

pub struct Fmt<F>(pub F)
where
    F: Fn(&mut fmt::Formatter, Config) -> fmt::Result;

impl<F> fmt::Display for Fmt<F>
where
    F: Fn(&mut fmt::Formatter, Config) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(
            f,
            Config {
                format_type: FormatType::Display,
            },
        )
    }
}

impl<F> fmt::Debug for Fmt<F>
where
    F: Fn(&mut fmt::Formatter, Config) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self.0)(
            f,
            Config {
                format_type: FormatType::Debug,
            },
        )
    }
}

pub struct FmtMut<F>(pub Box<RwLock<F>>)
where
    F: FnMut(&mut fmt::Formatter) -> fmt::Result;

impl<F> FmtMut<F>
where
    F: FnMut(&mut fmt::Formatter) -> fmt::Result,
{
    pub fn new(f: F) -> Self {
        FmtMut(Box::new(RwLock::new(f)))
    }
}

impl<F> fmt::Display for FmtMut<F>
where
    F: FnMut(&mut fmt::Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut p = self.0.write().unwrap();
        p(f)
    }
}

impl<F> fmt::Debug for FmtMut<F>
where
    F: FnMut(&mut fmt::Formatter) -> fmt::Result,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut p = self.0.write().unwrap();
        p(f)
    }
}

pub trait NodeDisplay {
    fn fmt(&self, f: &mut fmt::Formatter, config: &Config) -> fmt::Result;
}

pub trait AsTrait<U> {
    fn as_trait(&self) -> &dyn TreeDisplay<U>;
}

impl<T: TreeDisplay<U> + Sized, U> AsTrait<U> for T {
    fn as_trait(&self) -> &dyn TreeDisplay<U> {
        self
    }
}

pub trait TreeDisplay<U = ()>: NodeDisplay + AsTrait<U> {
    fn num_children(&self, cfg: &Config) -> usize;
    fn child_at(&self, index: usize, cfg: &Config) -> Option<&dyn TreeDisplay<U>>;
    fn child_at_bx<'a>(&'a self, _index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<U> + 'a> {
        panic!("This type doesn't used box values!")
    }

    fn get_user_data(&self) -> Option<U> {
        None
    }

    fn semantic_type(&self) -> SemanticType {
        SemanticType::Default
    }

    fn write(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        index: u32,
        indent: &String,
        last: bool,
        cfg: &Config,
    ) -> std::fmt::Result {
        write!(f, "{indent}")?;
        if index != 0 {
            write!(f, "{}", if last { "└──" } else { "├──" })?;
        }
        let nindent = format!(
            "{}{}",
            indent,
            if index == 0 {
                ""
            } else if last {
                "    "
            } else {
                "│   "
            }
        );

        self.fmt(f, &cfg)?;
        writeln!(f)?;

        // write!(f, "{}\n", self)?;

        let n = self.num_children(&cfg);
        for i in 0..n {
            let child = self.child_at(i, &cfg);
            if let Some(child) = child {
                child.write(f, (i + 1).try_into().unwrap(), &nindent, i == n - 1, cfg)?;
            } else {
                let child = self.child_at_bx(i, &cfg);
                child.write(f, (i + 1).try_into().unwrap(), &nindent, i == n - 1, cfg)?;
            }
        }

        write!(f, "")
    }

    fn write_unformatted(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        index: u32,
        indent: &String,
        last: bool,
        founc: &mut Box<dyn FnMut(&dyn TreeDisplay<U>, &str) -> Option<String>>,
    ) -> std::fmt::Result {
        write!(f, "{indent}")?;
        if index != 0 {
            write!(f, "{}", if last { "└──" } else { "├──" })?;
        }
        let nindent = format!(
            "{}{}",
            indent,
            if index == 0 {
                ""
            } else if last {
                "    "
            } else {
                "│   "
            }
        );

        let cfg = Config {
            format_type: FormatType::Display,
        };

        let val = format!("{}", Fmt(|f, cfg| self.fmt(f, &cfg)));
        let valo = founc(self.as_trait(), &val);
        if let Some(val) = valo {
            writeln!(f, "{val}")?;
        } else {
            writeln!(f, "{val}")?;
        }
        // self.fmt(f)?;
        // write!(f, "\n")?;

        let n = self.num_children(&cfg);
        for i in 0..n {
            let child = self.child_at(i, &cfg);
            if let Some(child) = child {
                child.write_unformatted(
                    f,
                    (i + 1).try_into().unwrap(),
                    &nindent,
                    i == n - 1,
                    founc,
                )?;
            } else {
                let child = self.child_at_bx(i, &cfg);
                child.write_unformatted(
                    f,
                    (i + 1).try_into().unwrap(),
                    &nindent,
                    i == n - 1,
                    founc,
                )?;
            }
        }

        write!(f, "")
    }

    fn format(&self) -> String {
        format!(
            "{}",
            Fmt(|f, cfg| self.write(f, 0, &String::from(""), false, &cfg))
        )
    }

    fn debug_format(&self) -> String {
        format!(
            "{:?}",
            Fmt(|f, cfg| self.write(f, 0, &String::from(""), false, &cfg))
        )
    }

    fn format_unformat(
        &self,
        mut founc: Box<dyn FnMut(&dyn TreeDisplay<U>, &str) -> Option<String>>,
    ) -> String {
        format!(
            "{}",
            FmtMut::new(|f| self.write_unformatted(f, 0, &String::from(""), false, &mut founc))
        )
    }

    fn semantic_format(&self) -> String {
        self.format_unformat(Box::new(|a, b| {
            match a.semantic_type() {
                SemanticType::Type => Some(b.bright_yellow().to_string()),
                SemanticType::Variable => Some(b.purple().to_string()),
                SemanticType::Flow => Some(b.magenta().to_string()),
                SemanticType::Function => Some(b.yellow().to_string()),
                SemanticType::Keyword => Some(b.blue().to_string()),
                SemanticType::Literal => Some(b.bright_green().to_string()),
                SemanticType::String => Some(b.bright_red().to_string()),
                SemanticType::Module => Some(b.bright_green().to_string()),
                // SemanticType::Variable => Some(b.truecolor(189, 183, 107).to_string()),
                // SemanticType::Flow => Some(b.truecolor(216, 160, 223).to_string()),
                // SemanticType::Function => Some(b.truecolor(255, 128, 0).to_string()),
                // SemanticType::Keyword => Some(b.truecolor(86, 156, 214).to_string()),
                // SemanticType::Literal => Some(b.truecolor(181, 206, 168).to_string()),
                // SemanticType::String => Some(b.truecolor(206, 144, 120).to_string()),
                // SemanticType::Module => Some(b.truecolor(181, 206, 168).to_string()),
                _ => None,
            }
        }))
    }
}

pub struct GrouperIter<'a, I: Iterator<Item = &'a dyn TreeDisplay>>(pub String, pub usize, pub I);

impl<'a, I: Iterator<Item = &'a dyn TreeDisplay>> NodeDisplay for GrouperIter<'a, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'a, I: Iterator<Item = &'a dyn TreeDisplay> + Clone> TreeDisplay for GrouperIter<'a, I> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.1
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        self.2.clone().nth(index)
    }
}

pub struct BoxedGrouperIter<'a, I: Iterator<Item = Box<dyn TreeDisplay + 'a>>>(
    pub String,
    pub usize,
    pub I,
);

impl<'b, I: Iterator<Item = Box<dyn TreeDisplay + 'b>>> NodeDisplay for BoxedGrouperIter<'b, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'b, I: Iterator<Item = Box<dyn TreeDisplay + 'b>> + Clone> TreeDisplay
    for BoxedGrouperIter<'b, I>
{
    fn num_children(&self, _cfg: &Config) -> usize {
        self.1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        None
    }

    fn child_at_bx<'a>(&'a self, _index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<()> + 'a> {
        self.2.clone().nth(_index).unwrap()
    }
}

pub struct Grouper<'a>(pub String, pub &'a dyn TreeDisplay);

impl<'a> NodeDisplay for Grouper<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<'a> TreeDisplay for Grouper<'a> {
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        Some(self.1)
    }
}

pub struct BoxedGrouper<'a>(pub String, pub Box<dyn TreeDisplay + 'a>);

impl NodeDisplay for BoxedGrouper<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl TreeDisplay for BoxedGrouper<'_> {
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        Some(&*self.1)
    }
}

pub struct BoxRef<'a>(pub Box<dyn TreeDisplay + 'a>);

impl NodeDisplay for BoxRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        self.0.fmt(f, _cfg)
    }
}

impl TreeDisplay for BoxRef<'_> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.0.num_children(_cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        self.0.child_at(index, _cfg)
    }

    fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<()> + 'a> {
        self.0.child_at_bx(index, _cfg)
    }
}

pub struct RfGrouper<T>(pub String, pub Rf<T>);

impl<T> NodeDisplay for RfGrouper<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl<T, U> TreeDisplay<U> for RfGrouper<T>
where
    T: TreeDisplay<U> + NodeDisplay,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<U>> {
        None
    }

    fn child_at_bx<'a>(&'a self, _index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<U> + 'a> {
        // Rf<dyn TreeDisplay<U>>::child_at_bx(&self.1, index)
        Box::new(self.1.borrow())
    }
}

pub struct StringFormatter(pub String);

impl NodeDisplay for StringFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, _cfg: &Config) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TreeDisplay for StringFormatter {
    fn num_children(&self, _cfg: &Config) -> usize {
        0
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        panic!()
    }
}

impl<'a, T: NodeDisplay + 'a> NodeDisplay for Vec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("")
    }
}

impl<'a, T: TreeDisplay + 'a> TreeDisplay for Vec<T> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.len()
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        Some(&self[index])
    }
}

impl<'a, K: NodeDisplay + 'a, V: NodeDisplay + 'a> NodeDisplay for HashMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("HashMap")
    }
}

impl<'b, K: TreeDisplay + 'b, V: TreeDisplay + 'b> TreeDisplay for HashMap<K, V> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.len()
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        // if let Some((key, value))
        None
    }

    fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'a> {
        let (name, item) = self.iter().nth(index).unwrap();
        Box::new(GrouperIter(
            "Entry".to_string(),
            2,
            [name as &dyn TreeDisplay, item as &dyn TreeDisplay].into_iter(),
        ))
    }
}

// impl<'a, T: NodeDisplay + 'a> NodeDisplay for HashMap<String, T> {
//     fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
//         f.write_str("HashMap")
//     }
// }

// impl<'b, T: TreeDisplay + 'b> TreeDisplay for HashMap<String, T> {
//     fn num_children(&self, _cfg: &Config) -> usize {
//         self.len()
//     }

//     fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
//         None
//     }

//     fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'a> {
//         let (name, item) = self.iter().nth(index).unwrap();
//         Box::new(Grouper(name.clone(), item))
//     }
// }

impl<'a, T: NodeDisplay + 'a> NodeDisplay for LinkedHashMap<String, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str("LinkedHashMap")
    }
}

impl<'b, T: TreeDisplay + 'b> TreeDisplay for LinkedHashMap<String, T> {
    fn num_children(&self, _cfg: &Config) -> usize {
        self.len()
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        None
    }

    fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'a> {
        let (name, item) = self.iter().nth(index).unwrap();
        Box::new(Grouper(name.clone(), item))
    }
}

// impl<'a, T: NodeDisplay + 'a> NodeDisplay for HashMap<String, T> {
//     fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
//         f.write_str("HashMap")
//     }
// }

// impl<'b, T: TreeDisplay + 'b> TreeDisplay for HashMap<String, T> {
//     fn num_children(&self, _cfg: &Config) -> usize {
//         self.len()
//     }

//     fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
//         None
//     }

//     fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay + 'a> {
//         let (name, item) = self.iter().nth(index).unwrap();
//         Box::new(Grouper(name.clone(), item))
//     }
// }

impl NodeDisplay for String {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str(self)
    }
}

impl TreeDisplay for String {
    fn num_children(&self, _cfg: &Config) -> usize {
        0
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        None
    }
}

impl NodeDisplay for usize {
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

impl TreeDisplay for usize {
    fn num_children(&self, _cfg: &Config) -> usize {
        0
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        None
    }
}

impl<T> NodeDisplay for Option<T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        match self {
            Some(v) => v.fmt(f, _cfg),
            _ => f.write_str(""),
        }
    }
}

impl<T> NodeDisplay for Rf<T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        self.borrow().fmt(f, _cfg)
    }
}

impl<T, U> TreeDisplay<U> for Rf<T>
where
    T: NodeDisplay + TreeDisplay<U>,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        1
    }

    fn child_at(&self, _index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<U>> {
        None
    }

    fn child_at_bx<'a>(&'a self, _index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<U> + 'a> {
        Box::new(self.borrow())
    }
}

impl<T> NodeDisplay for Ref<'_, T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(self, f, _cfg)
    }
}

impl<T> TreeDisplay for Ref<'_, T>
where
    T: NodeDisplay + TreeDisplay,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        <T as TreeDisplay>::num_children(self, _cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        <T as TreeDisplay>::child_at(self, index, _cfg)
    }
}

impl<T> NodeDisplay for MutexGuard<'_, T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(self, f, _cfg)
    }
}

impl<T> TreeDisplay for MutexGuard<'_, T>
where
    T: NodeDisplay + TreeDisplay,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        <T as TreeDisplay>::num_children(self, _cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        <T as TreeDisplay>::child_at(self, index, _cfg)
    }
}

impl<T> NodeDisplay for RwLockReadGuard<'_, T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(self, f, _cfg)
    }
}

impl<T, U> TreeDisplay<U> for RwLockReadGuard<'_, T>
where
    T: NodeDisplay + TreeDisplay<U>,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        <T as TreeDisplay<U>>::num_children(self, _cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<U>> {
        <T as TreeDisplay<U>>::child_at(self, index, _cfg)
    }

    fn child_at_bx<'a>(&'a self, index: usize, _cfg: &Config) -> Box<dyn TreeDisplay<U> + 'a> {
        <T as TreeDisplay<U>>::child_at_bx(self, index, _cfg)
    }

    fn get_user_data(&self) -> Option<U> {
        <T as TreeDisplay<U>>::get_user_data(self)
    }
}

impl<T> NodeDisplay for RefCell<T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(&self.borrow(), f, _cfg)
    }
}

impl<T> NodeDisplay for Mutex<T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(&self.lock().unwrap(), f, _cfg)
    }
}

impl<T> NodeDisplay for Box<T>
where
    T: NodeDisplay,
{
    fn fmt(&self, f: &mut std::fmt::Formatter, _cfg: &Config) -> std::fmt::Result {
        <T as NodeDisplay>::fmt(self.as_ref(), f, _cfg)
    }
}

impl<T> TreeDisplay for Box<T>
where
    T: NodeDisplay + TreeDisplay,
{
    fn num_children(&self, _cfg: &Config) -> usize {
        <T as TreeDisplay>::num_children(self.as_ref(), _cfg)
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay> {
        <T as TreeDisplay>::child_at(self.as_ref(), index, _cfg)
    }

    fn child_at_bx<'a>(&'a self, index: usize, cfg: &Config) -> Box<dyn TreeDisplay<()> + 'a> {
        <T as TreeDisplay>::child_at_bx(self.as_ref(), index, cfg)
    }
}

impl<A: NodeDisplay, B: NodeDisplay> NodeDisplay for (A, B) {
    fn fmt(&self, f: &mut fmt::Formatter, _config: &Config) -> fmt::Result {
        f.write_str("Tuple: 2")
    }
}

impl<A: TreeDisplay, B: TreeDisplay> TreeDisplay for (A, B) {
    fn num_children(&self, _cfg: &Config) -> usize {
        2
    }

    fn child_at(&self, index: usize, _cfg: &Config) -> Option<&dyn TreeDisplay<()>> {
        match index {
            0 => Some(&self.0),
            1 => Some(&self.1),
            _ => None,
        }
    }
}

pub trait AsTree {
    fn as_tree(&self) -> &dyn TreeDisplay;
    fn map_tree(&self) -> Option<&dyn TreeDisplay> {
        None
    }
}

impl<T> AsTree for Option<Box<T>>
where
    T: TreeDisplay,
{
    fn as_tree(&self) -> &dyn TreeDisplay {
        self.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f).unwrap()
    }

    fn map_tree(&self) -> Option<&dyn TreeDisplay> {
        self.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f)
    }
}

// impl <T> AsTree for Option<T> where T: TreeDisplay {
//     fn as_tree(&self) -> &dyn TreeDisplay {
//         self.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f).unwrap()
//     }

//     fn map_tree(&self) -> Option<&dyn TreeDisplay> {
//         self.as_ref().map::<&dyn TreeDisplay, _>(|f| &**f)
//     }
// }
