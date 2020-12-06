Day 2
=====

The second day's challenge was fairly easy to solve. The most natural approach to solve both parts is to use regular expressions in order to extract the relevant pieces of information and validate each entry. I'm using Advent of Code mostly as an excuse to practice and learn more about Rust, so I end up spending more time with the details of the language itself rather than coming up with a solution.

For regular expressions, I used the [regex](https://docs.rs/regex/1.4.2/regex/index.html) crate. It's pretty straightforward to use and the documentation also provides some nice examples. I also used the [lazy_static](https://docs.rs/lazy_static/1.4.0/lazy_static/) crate, in order to avoid compiling the same Regex over and over.

Here is what my password validation function looks like:

```rust
use lazy_static::lazy_static;
use regex::Regex;

type CheckerFn = Fn(usize,usize,char,String) -> bool;

fn is_valid_password(password: &str, checker: &CheckerFn) -> bool {
    lazy_static! {
        static ref RE: Regex::new(r"^(\d+)-(\d+) (\w{1}): (\w+)").unwrap();
    }

    match RE.captures(password) {
        None => panic!("Malformed entry: {}", password),
        Some(c) => {
            let _min = usize::from_str(c.get(1).unwrap().as_str()).unwrap();
            let _max = usize::from_str(c.get(2).unwrap().as_str()).unwrap();
            let letter = char::from_str(c.get(3).unwrap().as_str()).unwrap();
            let passwd = c.get(4).unwrap().as_str();

            checker(_min, _max, letter, passwd.to_string())
        }
    }
}
```

The `checker` is responsible for implementing the validation logic for each part of the problem.
