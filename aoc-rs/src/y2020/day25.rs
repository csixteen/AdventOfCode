use aoc::Solver;

fn calculate_loop_size(subj: i64, pubkey: i64) -> usize {
    let mut l = 1;
    let mut val = subj;

    loop {
        if val == pubkey {
            return l;
        }
        val = (val * subj) % 20201227;
        l += 1;
    }
}

fn generate_encryption_key(loop_size: usize, pubkey: i64) -> i64 {
    (0..loop_size).fold(1, |acc, _| (acc * pubkey) % 20201227)
}

fn combo_breaker<'a>(pubkey1: i64, pubkey2: i64) -> Result<i64, &'a str> {
    let loop1 = calculate_loop_size(7, pubkey1);
    let key1 = generate_encryption_key(loop1, pubkey2);
    let loop2 = calculate_loop_size(7, pubkey2);
    let key2 = generate_encryption_key(loop2, pubkey1);

    if key1 == key2 {
        Ok(key1)
    } else {
        Err("Keys don't match")
    }
}

const CARD_PUBKEY: i64 = 11239946;
const DOOR_PUBKEY: i64 = 10464955;

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, _input: &[&str]) -> String {
        println!("{:?}", combo_breaker(CARD_PUBKEY, DOOR_PUBKEY));

        String::new()
    }

    fn part2(&self, _input: &[&str]) -> String {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_calculate_loop_size() {
        assert_eq!(8, calculate_loop_size(7, 5764801));
        assert_eq!(11, calculate_loop_size(7, 17807724));
    }

    #[test]
    fn test_generate_encryption_key() {
        assert_eq!(14897079, generate_encryption_key(8, 17807724));
        assert_eq!(14897079, generate_encryption_key(11, 5764801));
    }
}
