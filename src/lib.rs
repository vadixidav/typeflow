//! `<typegroup> = <expression>`
//! A equation is a declarative statement of explicit conversion from an,
//! expression to a type group, but also an implicit conversion from the
//! typegroup to the expression. For instance `a b c = d e` makes it so
//! `a b c -> d e`. This means `a b c -> d` and `a b c -> e` are both implicit.
//!
//! `vector2 = x y` explicitly defines that a `vector2` can be
//! a type group of x and y inputs with no transformation. However, it also
//! implicitly defines the reverse operation, which is `vector2 -> x y`.
//! This means that a vector2 can be substituted anywhere an `x y` is required
//! and it will perform an automatic type conversion. Now we can also define
//! `x y = vector2` and this implies an explicit conversion to `x` or `y` is
//! possible, but it also implies that `x y -> vector2`. Since this operation
//! of implying `x y <-> vector2` is going to be common, a special syntax
//! is employed, which is `x y == vector2` or `vector2 == x y`. The left
//! side of such an expression is still required to be a type group, but
//! the right side is not.
//!
//! An example of a type group being equated (`=`) with an expression is
//! `a = +(b c)`. In this case, Typeflow also recognizes the following:
//! - `a c -> b`
//! - `a b -> c`
//! Although creating `a` from `b c` is an explicit conversion, in a normal
//! equate operation, the right side is considered less specific than the left.
//! This means that an implicit conversion that requires even some of the types
//! from the right side to convert the left side into the right is still a
//! valid implicit conversion. Here is the logical steps Typeflow will use
//! to deduce `a c -> b`:
//! - `a` can be turned into `+(b c)` (`a -> +(b c)`)
//! - `c` can be turned into `-(c)` (builtin `n == -(n)` if `n = i`)
//! - `a c` can be turned into `+(b c) -(c)`
//! - `+(b c) -(c)` can be turned into `+(b)`
//!     (builtin `n == n(+(+(n m) -(m)))` if `n = i` and `m = i`)
//! - `+(b)` can be turned into `b` (builtin `n == +(n)` if `n = i`)
//! As can be seen, lots of different things can be deduced from just `a c`.
//! However, in this case, having `b c` allows the deduction of almost nothing
//! aside from unary transformations of itself like `-(b) c`, `b -(c)` or
//! `-(b) -(c)`. This is because the conversion to `a` is explicit, but the
//! reverse transformation is implicit and can be automatically used by
//! Typeflow to make inferences.
//!
//! Problems:
//! - Any equation which introduces ambiguity must give an error. This
//!     should be added to the implementation as early as possible

#[macro_use]
extern crate nom;

use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
enum Plurality {
    One,
    Many,
}

/// How many times each type appears. If it is more than once, then wherever
/// it is consumed, it must be consumed as many inputs. Once of any type can
/// be coerced into a many of the same type.
type TypeGroup = HashMap<String, Plurality>;

/// An expression is a series of types and explicit type casts.
struct Expression {
    types: TypeGroup,
    casts: Vec<Cast>,
}

impl Expression {
    /// Finds the leaves of the expression tree to find exactly
    /// which types are being required as input.
    fn input_types() -> TypeGroup {
        unimplemented!()
    }

    /// Finds which types are output by the expression.
    fn output_types() -> TypeGroup {
        unimplemented!()
    }
}

/// An explicit conversion from one expression to a type.
struct Cast {
    typename: String,
    expression: Expression,
}

struct Equation {
    typegroup: TypeGroup,
    expression: Expression,
}

struct State {
    types: TypeGroup,
}

named!(parse_expression<&str, Cast>, unimplemented!());

/// Attempts to parse a cast inside of a line.
//named!(parse_cast<&str, Cast>, ws!(fold_many1!(ws!(take_till1_s!(|c| c.is_whitespace(), )))));

/// Attempts to parse a line into an explicit conversion.
//named!(parse_conversion<&str, Conversion>);

#[test]
fn add() {}
