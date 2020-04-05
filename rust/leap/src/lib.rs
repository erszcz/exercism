pub fn is_leap_year(year: u64) -> bool {
    // on every year that is evenly divisible by 4
    //   except every year that is evenly divisible by 100
    //     unless the year is also evenly divisible by 400
    match (is_divisible(year, 4), is_divisible(year, 100), is_divisible(year, 400)) {
        (true, true, true) => true,
        (true, true, _) => false,
        (true, _, _) => true,
        _ => false
    }
}

fn is_divisible(n: u64, div: u64) -> bool {
    n % div == 0
}
