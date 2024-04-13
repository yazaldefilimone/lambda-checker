use std::rc::Rc;

use crate::ast::Ast;
use crate::ast::{annotation, application, base_type, lambda, pi, variable};
use crate::types::ContextType;

pub struct Checker {
  pub prefix_name: char,
  pub counter: i32,
}

impl Checker {
  pub fn create_name(&mut self) -> String {
    let name = format!("{}{}", self.prefix_name, self.counter);
    self.counter += 1;
    name
  }
  pub fn evaluate(&mut self, expression: Rc<Ast>) -> Rc<Ast> {
    match &*expression {
      Ast::Application { function, argument } => {
        let function = self.evaluate(function.clone());
        let argument = self.evaluate(argument.clone());
        match &*function {
          Ast::Lambda { parameter, body } => self.substitute(body.clone(), parameter.clone(), argument),
          _ => expression,
        }
      }
      Ast::Annotation { term, _type } => term.clone(),
      _ => expression,
    }
  }

  pub fn substitute(&mut self, expression: Rc<Ast>, var: String, repl: Rc<Ast>) -> Rc<Ast> {
    match &*expression {
      Ast::Variable { name } if name == &var => repl,
      Ast::Application { function, argument } => {
        let function = self.substitute(function.clone(), var.clone(), repl.clone());
        let argument = self.substitute(argument.clone(), var.clone(), repl.clone());
        application(function, argument)
      }
      Ast::Lambda { parameter, body } if parameter != &var => {
        if repl.free_variable().contains(parameter) {
          let name = self.create_name();
          let new_variable = variable(&name);
          let body = self.substitute(body.clone(), parameter.clone(), new_variable);
          return Rc::new(Ast::Lambda { parameter: name, body });
        }
        let body = self.substitute(body.clone(), var, repl);
        lambda(parameter, body)
      }
      Ast::Annotation { term, _type } => {
        let term = self.substitute(term.clone(), var.clone(), repl.clone());
        let _type = self.substitute(_type.clone(), var, repl);
        annotation(term, _type)
      }
      Ast::Pi { parameter, _type, body } => {
        let (_paramenter, _body) = if repl.free_variable().contains(parameter) {
          let name = self.create_name();
          let new_variable = Rc::new(Ast::Variable { name: name.clone() });
          let body = self.substitute(body.clone(), parameter.clone(), new_variable.clone());
          (name, body)
        } else {
          (parameter.clone(), body.clone())
        };

        let _type = self.substitute(_type.clone(), var.clone(), repl.clone());
        let body = if _paramenter == *var {
          _body
        } else {
          self.substitute(_body.clone(), var.clone(), repl.clone())
        };
        pi(&_paramenter, _type, body)
      }
      _ => expression.clone(),
    }
  }

  pub fn reduce(&mut self, expression: Rc<Ast>) -> Rc<Ast> {
    match &*expression {
      Ast::Application { function, argument } => {
        let _function = self.reduce(function.clone());
        let _argument = self.reduce(argument.clone());
        match &*_function {
          Ast::Lambda { parameter, body } => {
            let _result = self.substitute(body.clone(), parameter.clone(), _argument.clone());
            self.reduce(_result)
          }
          _ => application(_function, _argument),
        }
      }
      Ast::Annotation { term, _type } => self.reduce(term.clone()),
      Ast::Lambda { parameter, body } => {
        let _body = self.reduce(body.clone());
        lambda(parameter, _body)
      }
      Ast::Pi { parameter, _type, body } => {
        let _type = self.reduce(_type.clone());
        let body = self.reduce(body.clone());
        pi(parameter, _type, body)
      }
      _ => expression.clone(),
    }
  }

  pub fn check(&mut self, ctx: ContextType, expression: Rc<Ast>, _type: Rc<Ast>) {
    let expected = self.evaluate(_type);
    // check if the expression is a lambda and expected is a pi, (maybe I should use a match here... hum nahhh :P)
    if let Ast::Lambda { parameter, body } = &*expression {
      if let Ast::Pi { parameter: pi_parameter, _type, body: pi_body } = &*expected {
        let mut new_ctx = ctx.clone();
        // Γ, x : A
        new_ctx.insert(parameter.clone(), _type.clone());
        // B[x := x]
        let ret_type = self.substitute(pi_body.clone(), pi_parameter.clone(), variable(&parameter));
        // B[x := x] ⊢ x => A

        return self.check(new_ctx, body.clone(), ret_type);
      }
    }

    let infered = self.infer(ctx, expression.clone());
    if !self.compare(expected.clone(), infered.clone()) {
      println!("checking {:?} ˜˜˜ {:?}", expression, expected);
      panic!("type error: expected {:?}, got {:?}", expected, infered);
    }
  }

  pub fn compare(&mut self, left: Rc<Ast>, right: Rc<Ast>) -> bool {
    let eval_left = self.evaluate(left.clone());
    let eval_right = self.evaluate(right.clone());
    match (&*eval_left, &*eval_right) {
      (Ast::Variable { name: var_left }, Ast::Variable { name: var_right }) => var_left == var_right,

      (
        Ast::Application { function: fun_left, argument: arg_left },
        Ast::Application { function: fun_right, argument: arg_right },
      ) => self.compare(fun_left.clone(), fun_right.clone()) && self.compare(arg_left.clone(), arg_right.clone()),

      (
        Ast::Lambda { parameter: param_left, body: body_left },
        Ast::Lambda { parameter: param_right, body: body_right },
      ) => {
        let name = self.create_name();
        let body_left = self.substitute(body_left.clone(), name.clone(), variable(param_left));
        let body_right = self.substitute(body_right.clone(), name, variable(param_right));
        self.compare(body_left, body_right)
      }

      (
        Ast::Pi { parameter: param_left, _type: type_left, body: body_left },
        Ast::Pi { parameter: param_right, _type: type_right, body: body_right },
      ) => {
        let name = self.create_name();
        let body_left = self.substitute(body_left.clone(), name.clone(), variable(param_left));
        let body_right = self.substitute(body_right.clone(), name, variable(param_right));
        self.compare(body_left, body_right) && self.compare(type_left.clone(), type_right.clone())
      }

      (Ast::Annotation { term: left_term, _type: left_type }, Ast::Annotation { term, _type }) => {
        self.compare(left_term.clone(), term.clone()) && self.compare(left_type.clone(), _type.clone())
      }
      (Ast::Type, Ast::Type) => true,
      _ => false,
    }
  }

  pub fn infer(&mut self, env: ContextType, expression: Rc<Ast>) -> Rc<Ast> {
    match &*expression {
      Ast::Lambda { .. } => panic!("type error: cannot infer the type of a lambda"),
      // Γ ⊢ x => A
      Ast::Variable { name } => {
        // x : A ∈ Γ
        if let Some(t) = env.get(name) {
          t.clone()
        } else {
          panic!("type error: not found a variable with the name {}", name)
        }
      }
      // Γ ⊢ y z => B[x := z]
      Ast::Application { function, argument } => {
        // Γ ⊢ y => A -> B
        let _function = self.infer(env.clone(), function.clone());
        match &*_function {
          // A -> B
          Ast::Pi { parameter, _type, body } => {
            self.check(env.clone(), argument.clone(), _type.clone());
            // B[x := z]
            self.substitute(body.clone(), parameter.clone(), argument.clone())

            // panic!("type error: expected {:?}, got {:?}", _type, _argument);
          }
          _ => panic!(
            "type error: cannot apply to a non-function, applied {:?}, to {:?}",
            _function, argument
          ),
        }
      }
      // Γ ⊢ (x: A) -> B => Type
      Ast::Pi { parameter, _type, body } => {
        self.check(env.clone(), _type.clone(), base_type());
        let mut new_env = env.clone();
        new_env.insert(parameter.clone(), _type.clone());
        self.check(new_env, body.clone(), base_type());
        base_type()
      }

      Ast::Annotation { term, _type } => {
        self.check(env.clone(), _type.clone(), base_type());
        self.check(env, term.clone(), _type.clone());
        _type.clone()
      }
      Ast::Type => base_type(),
    }
  }
}
