use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use itertools::Itertools;
use lazy_static::lazy_static;
use regex::Regex;

use aoc::Solver;

type FieldsRanges = HashMap<String, Vec<(usize, usize)>>;
type Ticket = Vec<usize>;

fn parse_ranges(group: &Vec<String>) -> FieldsRanges {
    lazy_static! {
        static ref RULE: Regex = Regex::new(r"(.*): (\d+)\-(\d+) or (\d+)\-(\d+)").unwrap();
    }

    let mut ranges = HashMap::new();

    for line in group.iter() {
        match RULE.captures(line) {
            Some(g) => {
                ranges.insert(
                    g.get(1).unwrap().as_str().to_owned(),
                    vec![
                        (
                            usize::from_str(g.get(2).unwrap().as_str()).unwrap(),
                            usize::from_str(g.get(3).unwrap().as_str()).unwrap(),
                        ),
                        (
                            usize::from_str(g.get(4).unwrap().as_str()).unwrap(),
                            usize::from_str(g.get(5).unwrap().as_str()).unwrap(),
                        ),
                    ],
                );
            }
            None => panic!("Malformed rule"),
        }
    }

    ranges
}

fn parse_other_tickets(group: &Vec<String>) -> Vec<Ticket> {
    group[1..]
        .iter()
        .map(|ticket| {
            ticket
                .split(',')
                .map(|n| usize::from_str(n).unwrap())
                .collect()
        })
        .collect()
}

fn is_valid_field(value: usize, ranges: &Vec<(usize, usize)>) -> bool {
    ranges
        .iter()
        .find(|(start, end)| value >= *start && value <= *end)
        .is_some()
}

fn ticket_invalid_values(ticket: &Ticket, ranges: &Vec<(usize, usize)>) -> usize {
    ticket
        .iter()
        .filter(|&field| !is_valid_field(*field, ranges))
        .sum()
}

fn scanning_error_rate(other_tickets: &Vec<Ticket>, ranges: &FieldsRanges) -> usize {
    other_tickets.iter().fold(0, |acc, ticket| {
        acc + ticket_invalid_values(ticket, &ranges.values().flat_map(|v| v).cloned().collect())
    })
}

fn is_valid_ticket(ticket: &Ticket, ranges: &Vec<(usize, usize)>) -> bool {
    ticket.iter().all(|field| is_valid_field(*field, ranges))
}

fn valid_tickets(other_tickets: &Vec<Ticket>, ranges: &FieldsRanges) -> Vec<Ticket> {
    let vv = ranges.values().flat_map(|v| v.to_vec()).collect();

    other_tickets
        .iter()
        .filter(|&t| is_valid_ticket(t, &vv))
        .cloned()
        .collect()
}

fn candidate_indices(
    tickets: &Vec<Ticket>,
    ranges: &FieldsRanges,
) -> HashMap<String, HashSet<usize>> {
    let num_fields = tickets[0].len();
    let mut res = HashMap::new();

    for (field, r) in ranges.iter() {
        for i in 0..num_fields {
            if (0..tickets.len()).all(|j| is_valid_field(tickets[j][i], r)) {
                res.entry(field.to_string())
                    .or_insert(HashSet::new())
                    .insert(i);
            }
        }
    }

    res
}

fn fields_indices(tickets: &Vec<Ticket>, ranges: &FieldsRanges) -> HashMap<String, usize> {
    let mut candidates = candidate_indices(tickets, ranges);
    let mut assured: HashMap<String, usize> =
        candidates
            .iter()
            .filter(|(_, v)| v.len() == 1)
            .fold(HashMap::new(), |mut acc, (k, v)| {
                acc.insert(k.to_string(), *v.iter().next().unwrap());
                acc
            });

    assured.keys().for_each(|name| {
        candidates.remove(name);
    });

    while assured.len() < tickets[0].len() {
        let assured_values: Vec<usize> = assured.values().cloned().collect();
        let mut to_remove: Vec<String> = Vec::new();

        for v in assured_values.iter() {
            for (u_k, u_v) in candidates.iter_mut() {
                if u_v.remove(v) && u_v.len() == 1 {
                    assured.insert(u_k.to_string(), *u_v.iter().next().unwrap());
                    to_remove.push(u_k.to_string());
                }
            }
        }

        to_remove.iter().for_each(|name| {
            candidates.remove(name);
        });
    }

    assured
}

fn destination_fields_value(
    your_ticket: &Ticket,
    other_tickets: &Vec<Ticket>,
    ranges: &FieldsRanges,
) -> usize {
    let tickets = valid_tickets(other_tickets, ranges);
    let fi = fields_indices(&tickets, ranges);

    let mut res = 1;
    for (field, index) in fi.iter() {
        if field.len() >= 9 && &field[..9] == "departure" {
            res *= your_ticket[*index];
        }
    }

    res
}

pub struct Solution;

impl Solver for Solution {
    fn part1(&self, input: &[&str]) -> String {
        let groups = input
            .into_iter()
            .map(|&line| line.to_owned())
            .group_by(|line| !line.is_empty());
        let grouped: Vec<Vec<String>> = groups
            .into_iter()
            .map(|(_, group)| group.collect::<Vec<String>>())
            .filter(|g| !g[0].is_empty())
            .collect();
        let ranges = parse_ranges(&grouped[0]);
        let other_tickets = parse_other_tickets(&grouped[2]);
        scanning_error_rate(&other_tickets, &ranges).to_string()
    }

    fn part2(&self, input: &[&str]) -> String {
        let groups = input
            .into_iter()
            .map(|&line| line.to_owned())
            .group_by(|line| !line.is_empty());
        let grouped: Vec<Vec<String>> = groups
            .into_iter()
            .map(|(_, group)| group.collect::<Vec<String>>())
            .filter(|g| !g[0].is_empty())
            .collect();
        let your_ticket: Ticket = grouped[1][1]
            .split(',')
            .map(|n| usize::from_str(n).unwrap())
            .collect();
        let other_tickets = parse_other_tickets(&grouped[2]);
        let ranges = parse_ranges(&grouped[0]);
        destination_fields_value(&your_ticket, &other_tickets, &ranges).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_tickets() {
        let other_tickets = vec![
            vec![7, 3, 47],
            vec![40, 4, 50],
            vec![55, 2, 20],
            vec![38, 6, 12],
        ];
        let mut ranges: FieldsRanges = HashMap::new();
        ranges.insert("class".to_string(), vec![(1, 3), (5, 7)]);
        ranges.insert("row".to_string(), vec![(6, 11), (33, 44)]);
        ranges.insert("seat".to_string(), vec![(13, 40), (45, 50)]);

        assert_eq!(vec![vec![7, 3, 47]], valid_tickets(&other_tickets, &ranges),);
    }
}
