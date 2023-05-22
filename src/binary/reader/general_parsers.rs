use nom::{combinator::success, IResult, Parser};

/// Executes the parser `f` `count` times, executes the `separator` parser and
/// discards the output, then executes `g` `count` times and zips the results
/// with those of `f`.
pub fn separated_zip_count<I, O1, V, O2, F, S, G, E>(
    count: usize,
    mut f: F,
    mut separator: S,
    mut g: G,
) -> impl FnMut(I) -> IResult<I, Vec<(O1, O2)>, E>
where
    F: Parser<I, O1, E>,
    S: Parser<I, V, E>,
    G: Parser<I, O2, E>,
    I: Clone + PartialEq,
    O1: Clone,
    E: nom::error::ParseError<I>,
{
    move |i: I| {
        let mut input = i.clone();
        let mut res1 = Vec::with_capacity(count);
        let mut res = Vec::with_capacity(count);

        for _ in 0..count {
            let input_ = input.clone();
            match f.parse(input_) {
                Ok((i, o1)) => {
                    res1.push(o1);
                    input = i;
                }
                Err(nom::Err::Error(e)) => {
                    return Err(nom::Err::Error(E::append(
                        i,
                        nom::error::ErrorKind::Count,
                        e,
                    )));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        let (mut input, _) = separator.parse(input)?;

        let mut res1 = res1.iter();

        for _ in 0..count {
            let input_ = input.clone();
            match g.parse(input_) {
                Ok((i, o2)) => {
                    res.push((res1.next().unwrap().clone(), o2));
                    input = i;
                }
                Err(nom::Err::Error(e)) => {
                    return Err(nom::Err::Error(E::append(
                        i,
                        nom::error::ErrorKind::Count,
                        e,
                    )));
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok((input, res))
    }
}

/// Executes the parser `f` `count` times, then executes `g` `count` times and
/// zips the results with those of `f`.
pub fn _zip_count<I, O1, O2, F, G, E>(
    count: usize,
    f: F,
    g: G,
) -> impl FnMut(I) -> IResult<I, Vec<(O1, O2)>, E>
where
    F: Parser<I, O1, E>,
    G: Parser<I, O2, E>,
    I: Clone + PartialEq,
    O1: Clone,
    O2: Clone,
    E: nom::error::ParseError<I>,
{
    separated_zip_count(count, f, success(()), g)
}
