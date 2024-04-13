use std::rc::Rc;

use im::HashSet;

// dependent type lambda calculus AST
#[derive(Debug)]
pub enum Ast {
  Variable {
    name: String,
  },
  Application {
    function: Rc<Ast>,
    argument: Rc<Ast>,
  },
  Lambda {
    parameter: String,
    body: Rc<Ast>,
  },
  Annotation {
    term: Rc<Ast>,
    _type: Rc<Ast>,
  },
  Pi {
    parameter: String,
    _type: Rc<Ast>,
    body: Rc<Ast>,
  },
  Type,
}

impl Ast {
  pub fn free_variable(&self) -> HashSet<String> {
    let mut env = HashSet::new();

    fn scanner(ast: &Ast, ctx: im::HashSet<String>, env: &mut HashSet<String>) {
      match ast {
        Ast::Variable { name } => {
          if !ctx.contains(name) {
            env.insert(name.clone());
          }
        }
        Ast::Application { function, argument } => {
          scanner(function, ctx.clone(), env);
          scanner(argument, ctx, env);
        }
        Ast::Lambda { parameter, body } => {
          let mut _ctx = ctx.clone();
          _ctx.insert(parameter.clone());
          scanner(body, _ctx, env);
        }
        Ast::Annotation { term, _type } => {
          scanner(term, ctx.clone(), env);
          scanner(_type, ctx, env);
        }
        Ast::Pi { parameter, _type, body } => {
          let mut _ctx = ctx.clone();
          _ctx.insert(parameter.clone());
          scanner(_type, ctx.clone(), env);
          scanner(body, _ctx, env);
        }
        Ast::Type => {}
      }
    }

    scanner(self, Default::default(), &mut env);
    return env;
  }
}

pub fn variable(name: &str) -> Rc<Ast> {
  Rc::new(Ast::Variable { name: name.to_string() })
}

pub fn application(function: Rc<Ast>, argument: Rc<Ast>) -> Rc<Ast> {
  Rc::new(Ast::Application { function, argument })
}

pub fn lambda(parameter: &str, body: Rc<Ast>) -> Rc<Ast> {
  Rc::new(Ast::Lambda { parameter: parameter.to_string(), body })
}

pub fn annotation(term: Rc<Ast>, _type: Rc<Ast>) -> Rc<Ast> {
  Rc::new(Ast::Annotation { term, _type })
}

pub fn pi(parameter: &str, _type: Rc<Ast>, body: Rc<Ast>) -> Rc<Ast> {
  Rc::new(Ast::Pi { parameter: parameter.to_string(), _type, body })
}
pub fn base_type() -> Rc<Ast> {
  Rc::new(Ast::Type)
}
