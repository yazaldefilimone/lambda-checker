use crate::ast::Ast;
use std::rc::Rc;
pub type ContextType = im::HashMap<String, Rc<Ast>>;
