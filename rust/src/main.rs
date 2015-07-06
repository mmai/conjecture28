extern crate primal;

fn main() {
    let p = primal::Primes::all().nth(10001 - 1).unwrap();
    println!("The 10001st prime is {}", p); // 104743
}
