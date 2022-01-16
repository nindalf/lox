Parser improvements

- Right now I'm returning `Option<T>` everywhere. Instead, each matcher could return a Result<T, E> where the Error will be like `nom`'s Error -
  - no match (and so try the next option)
  - more input needed, or 
  - failure
- Right now all 3 options are being squashed into None, which means parsing is success/failure. No information is propagated anywhere. I'm sure the book gets to this stuff later, but I'd like to get it right first time around.
- Instead of reading a couple of characters and matching manually, I could use the `nom` approach of simply matching to each token matcher in sequence and seeing what succeeds. Each successful matcher returns the remaining string, so I don't need to keep track of offsets
- Should wrap the input string in a Span, like [`nom_locate`](https://docs.rs/nom_locate) does. This will give me location information.

Articles I read:

- [Parsing Text with Nom](https://blog.adamchalmers.com/nom-chars/). Parses inputs to AOC problems. I thought I could get started with that, because it's a small self contained problem. Before I fell down the rabbit hole of reading more and more.
- [Rust - Writing Parsers With nom Parser Combinator Framework](https://iximiuz.com/en/posts/rust-writing-parsers-with-nom/). Implements a parser for a DSL.
- [Error Management (official docs)](https://github.com/Geal/nom/blob/main/doc/error_management.md)
- [List of parsers and combinators (official docs)](https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md)

---

Learn more about

- `AsRef`


---

- I love how nom has been implemented. They define these traits like Offset and provide impls for all the types they use. Then a crate like nom_locate sets the trait bound as Offset and uses that method. Brilliant. That's just one trait, there's a bunch of others.
- I'm struggling with the borrow checker on my parser. I'm considering pausing, proceeding with nom and then coming back to this.