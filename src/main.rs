mod ast;
mod checker;
mod types;
use core::fmt::Display;

use ast::{annotation, Ast};
use ast::{application, base_type, lambda, pi, variable};
use checker::Checker;

impl Display for Ast {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Ast::Variable { name } => write!(f, "{}", name),
      Ast::Application { function, argument } => write!(f, "({} {})", function, argument),
      Ast::Lambda { parameter, body } => write!(f, "(λ{}.{})", parameter, body),
      Ast::Annotation { term, _type } => write!(f, "({} : {})", term, _type),
      Ast::Pi { parameter, _type, body } => write!(f, "({} : {}) -> {}", parameter, _type, body),
      Ast::Type => write!(f, "Type"),
    }
  }
}
fn main() {
  let mut checker = Checker { prefix_name: '_', counter: 0 };

  // type Nat : Type {
  //  zero : Nat
  //
  //  succ : Nat -> Nat
  // }

  let nat = pi(
    "nat",
    base_type(),
    pi(
      "zero",
      variable("nat"),
      pi("succ", pi("_", variable("nat"), variable("nat")), variable("nat")),
    ),
  );

  checker.check(Default::default(), nat.clone(), base_type());

  // zero
  // \_ : nat -> nat. zero
  let zero = lambda("_", lambda("x", lambda("y", variable("x"))));
  // zero : nat -> nat -> nat
  let zero = annotation(zero.clone(), nat.clone());
  // println!("{}", zero.clone());
  checker.infer(Default::default(), zero.clone());
  // succ
  // \n : nat. \f : nat -> nat. \x : nat. \y : nat. y (n f x y)
  let succ = lambda(
    "m",
    lambda(
      "ty",
      lambda(
        "z",
        lambda(
          "s",
          application(
            variable("s"),
            application(
              application(application(variable("m"), variable("ty")), variable("z")),
              variable("s"),
            ),
          ),
        ),
      ),
    ),
  );

  // succ : nat -> nat
  let succ = annotation(succ, pi("_", nat.clone(), nat.clone()));
  checker.infer(Default::default(), succ.clone());

  // add : nat -> nat -> nat
  // \m : nat. \n : nat. \ty : Type. \z : ty. \s : ty -> ty. m ty (n ty z s) s
  let add = lambda(
    "m",
    lambda(
      "n",
      lambda(
        "ty",
        lambda(
          "z",
          lambda(
            "s",
            application(
              application(
                application(variable("m"), variable("ty")),
                application(
                  application(application(variable("n"), variable("ty")), variable("z")),
                  variable("s"),
                ),
              ),
              variable("s"),
            ),
          ),
        ),
      ),
    ),
  );

  let add = annotation(add.clone(), pi("_", nat.clone(), pi("_", nat.clone(), nat.clone())));
  checker.infer(Default::default(), add.clone());

  // one = succ zero
  let one = application(succ.clone(), zero.clone());
  let two = application(succ.clone(), one.clone());
  let three = application(succ.clone(), two.clone());
  let four = application(succ.clone(), three.clone());
  let five = application(succ.clone(), four.clone());

  let add_five = application(application(add.clone(), three.clone()), two.clone());
  // checker.infer(Default::default(), add_five.clone÷());
  checker.check(Default::default(), add_five.clone(), nat.clone());

  let five_normalized = checker.reduce(five.clone());
  let add_five_normalized = checker.reduce(add_five.clone());

  let add_two_one = application(application(add.clone(), two.clone()), one.clone());
  let add_one_two = application(application(add.clone(), one.clone()), two.clone());
  println!("{}", checker.reduce(add_two_one.clone()));
  println!("five: {}", five_normalized);
  println!("five: {}", add_five_normalized);
  //  test add_two_one = add_one_two
  println!("two + one: {}", checker.compare(add_one_two, add_two_one));
  println!(
    "norm (five == five) : {}",
    checker.compare(five_normalized, add_five_normalized)
  );
  println!("add_five = five: {}", checker.compare(five, add_five));
}
