pub fn verse(n: u32) -> String {
    match n {
        0 => "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n".to_string(),
        1 => "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n".to_string(),
        _ => format!("{} bottles of beer on the wall, {} bottles of beer.\nTake it down and pass it around, {} bottle of beer on the wall.\n",
                     n, n, n-1)
    }
}

pub fn sing(start: u32, end: u32) -> String {
    unimplemented!("sing verses {} to {}, inclusive", start, end)
}
