use crate::error::{Error, ErrorKind};
use std::cmp;
use std::io;
use std::io::Write;
use std::ops;

#[derive(Clone)]
pub struct Matrix {
    cols: usize,
    rows: usize,
    mat: Vec<Vec<f64>>,
}

impl Matrix {
    pub fn make_o_mat(n: usize) -> Result<Self, Error> {
        if n == 0 {
            Err(Error::from(ErrorKind::NoCols))
        } else {
            let temp = vec![vec![0.0; n]; n];
            Ok(Matrix {
                cols: n,
                rows: n,
                mat: temp,
            })
        }
    }

    pub fn make_e_mat(n: usize) -> Result<Self, Error> {
        if n == 0 {
            Err(Error::from(ErrorKind::NoCols))
        } else {
            let mut temp = vec![vec![0.0; n]; n];
            for i in 0..n {
                temp[i][i] = 1.0;
            }
            Ok(Matrix {
                cols: n,
                rows: n,
                mat: temp,
            })
        }
    }

    pub fn make_from_vec_vec(vecvec: Vec<Vec<f64>>) -> Result<Self, Error> {
        let rows = vecvec.len();
        if rows == 0 {
            Err(Error::from(ErrorKind::NoRows))
        } else {
            let cols = vecvec[0].len();
            if cols == 0 {
                Err(Error::from(ErrorKind::NoCols))
            } else {
                for row in 1..rows {
                    if vecvec[row].len() != cols {
                        return Err(Error::from(ErrorKind::NoCols));
                    }
                    for value in &vecvec[row] {
                        if value.is_nan() || value.is_infinite() {
                            return Err(Error::from(ErrorKind::InfinityOrNan));
                        }
                    }
                }
                Ok(Matrix {
                    cols: cols,
                    rows: rows,
                    mat: vecvec,
                })
            }
        }
    }

    pub fn print(&self) {
        for col in &self.mat {
            for item in col {
                print!("{:8.3}", item);
            }
            print!("\n");
        }
    }

    pub fn scan_mat() -> Result<Matrix, Error> {
        let mut buffer: String;
        let stdin = io::stdin();
        let cols: usize;
        let rows: usize;
        loop {
            print!("the number of Rows and Cols= ");
            io::stdout().flush().unwrap();
            buffer = String::new();
            if let Err(e) = stdin.read_line(&mut buffer) {
                return Err(Error::new(ErrorKind::Io, e));
            }
            let temp = buffer.split_whitespace().collect::<Vec<_>>();
            if temp.len() != 2 {
                println!("Input two arguments");
                continue;
            }
            let r = match temp[0].parse::<usize>() {
                Ok(v) => v,
                Err(_) => {
                    println!("Invailed Argument");
                    continue;
                }
            };
            let c = match temp[1].parse::<usize>() {
                Ok(v) => v,
                Err(_) => {
                    println!("Invailed Argument");
                    continue;
                }
            };
            cols = c;
            rows = r;
            break;
        }
        let mut mat = Vec::default();
        let mut y = 0;
        'argument_check: while y < rows {
            println!("input {}{} row", y + 1, num_to_th(y + 1));
            buffer = String::new();
            if let Err(e) = stdin.read_line(&mut buffer) {
                return Err(Error::new(ErrorKind::Io, e));
            }
            let temp = buffer
                .split_whitespace()
                .map(|s| s.parse::<f64>())
                .collect::<Vec<_>>();
            if temp.len() != cols {
                if temp.len() < cols {
                    println!("element error -- too few");
                } else {
                    println!("element error -- too much");
                }
                continue;
            }
            let mut temp2 = Vec::default();
            for result in temp {
                match result {
                    Ok(v) if v.is_nan() || v.is_infinite() => {
                        println!("Cannot be nan or infinity");
                        continue 'argument_check;
                    }
                    Ok(v) => temp2.push(v),
                    Err(_) => {
                        println!("Invailed Argument");
                        continue 'argument_check;
                    }
                }
            }
            mat.push(temp2);
            y += 1;
        }
        Ok(Matrix {
            cols: cols,
            rows: rows,
            mat: mat,
        })
    }

    pub fn times(&self, value: f64) -> Result<Self, Error> {
        if value.is_nan() || value.is_infinite() {
            Err(Error::from(ErrorKind::InfinityOrNan))
        } else {
            let mut mat: Vec<Vec<f64>> = Vec::new();
            for y in 0..self.rows {
                let mut temp: Vec<f64> = Vec::new();
                for x in 0..self.cols {
                    let element = self.mat[y][x] * value;
                    if element.is_nan() || element.is_infinite() {
                        return Err(Error::from(ErrorKind::InfinityOrNan));
                    }
                    temp.push(element);
                }
                mat.push(temp);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }

    pub fn trans(&self) -> Self {
        let mut mat: Vec<Vec<f64>> = Vec::new();
        for y in 0..self.rows {
            let mut temp: Vec<f64> = Vec::new();
            for x in 0..self.cols {
                temp.push(self.mat[x][y]);
            }
            mat.push(temp);
        }
        Matrix {
            cols: self.rows,
            rows: self.cols,
            mat: mat,
        }
    }

    pub fn rank(&self) -> usize {
        let mut temp = self.clone();
        for i in 0..self.rows {
            let (k, j) = match temp.my_special_search_in_rank(i) {
                Some((x, y)) => (x, y),
                None => return i,
            };
            if i != j {
                temp = temp.row_switching_transformation(i, j).expect("安全なはず");
            }
            let value = 1.0 / temp.mat[i][k];
            temp = temp
                .row_multiplying_transformation(i, value)
                .expect("0除算も起こらないはず");
            for j in (i + 1)..self.rows {
                let value = temp.mat[j][k];
                temp = temp
                    .row_addition_transformation(i, j, -value)
                    .expect("大丈夫なはず");
            }
        }
        self.rows
    }

    pub fn inverse(&self) -> Result<Self, Error> {
        if self.cols != self.rows {
            Err(Error::from(ErrorKind::MissmMatchSizeOfMatrix))
        } else if self.rank() != self.rows {
            Err(Error::from(ErrorKind::SingularMatrix))
        } else {
            let mut temp = Matrix {
                cols: self.cols * 2,
                rows: self.rows,
                mat: vec![vec![0.0; self.cols * 2]; self.rows],
            };
            for y in 0..self.rows {
                for x in 0..self.cols {
                    temp.mat[y][x] = self.mat[y][x];
                }
            }
            for i in 0..self.rows {
                temp.mat[i][self.cols + i] = 1.0;
            }
            for i in 0..self.rows {
                let j = temp.my_special_search(i).expect("正則行列だから大丈夫");
                if i != j {
                    temp = temp
                        .row_switching_transformation(i, j)
                        .expect("絶対安全なはず");
                }
                let value = 1.0 / temp.mat[i][i];
                temp = temp
                    .row_multiplying_transformation(i, value)
                    .expect("おそらく0除算も起こらないはず");
                for j in (i + 1)..self.rows {
                    let value = temp.mat[j][i];
                    temp = temp
                        .row_addition_transformation(i, j, -value)
                        .expect("大丈夫なはず2");
                }
            }
            for i in (1..self.rows).rev() {
                for j in (0..i).rev() {
                    let value = temp.mat[j][i];
                    temp = temp
                        .row_addition_transformation(i, j, -value)
                        .expect("大丈夫なはず3");
                }
            }
            let mut ans_vec_vec = vec![];
            for y in 0..self.rows {
                let mut row = vec![];
                for x in 0..self.cols {
                    row.push(temp.mat[y][self.cols + x]);
                }
                ans_vec_vec.push(row);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: ans_vec_vec,
            })
        }
    }

    pub fn det(&self) -> Result<f64, Error> {
        if self.cols != self.rows {
            Err(Error::from(ErrorKind::MissmMatchSizeOfMatrix))
        }else {
            let mut temp = self.clone();
            for i in 0..self.rows {
                let j = match temp.my_special_search(i) {
                    Some(v) => v,
                    None => return Ok(0.0),
                };
                if i != j {
                    temp = temp.row_addition_transformation(j, i, 1.0).expect("きっと大丈夫じゃない");
                }
                for j in (i + 1)..self.rows {
                    let value = temp.mat[j][i]/temp.mat[i][i];
                    temp = temp
                        .row_addition_transformation(i, j, -value)
                        .expect("全然大丈夫じゃない");
                }
            }
            let mut answer = 1.0;
            for i in 0..self.rows {
                answer *= temp.mat[i][i];
            }
            Ok(answer)
        }
    }

    fn my_special_search_in_rank(&self, n: usize) -> Option<(usize, usize)> {
        if n >= self.cols || n >= self.rows {
            None
        } else {
            for x in n..self.cols {
                for y in n..self.rows {
                    if self.mat[y][x].abs() >= 1e-10 {
                        return Some((x, y));
                    }
                }
            }
            None
        }
    }

    fn my_special_search(&self, n: usize) -> Option<usize> {
        if n >= self.cols || n >= self.rows {
            None
        } else {
            for y in n..self.rows {
                if self.mat[y][n].abs() >= 1e-10 {
                    return Some(y);
                }
            }
            None
        }
    }

    fn row_switching_transformation(self, i: usize, j: usize) -> Result<Self, Error> {
        if i >= self.rows || j >= self.rows {
            Err(Error::from(ErrorKind::IndexOutOfBounds))
        } else {
            let mut mat = vec![];
            for y in 0..self.rows {
                let raw = if y == i {
                    self.mat[j].clone()
                } else if y == j {
                    self.mat[i].clone()
                } else {
                    self.mat[y].clone()
                };
                mat.push(raw);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }

    fn row_multiplying_transformation(self, i: usize, value: f64) -> Result<Self, Error> {
        if i >= self.rows {
            Err(Error::from(ErrorKind::IndexOutOfBounds))
        } else if value.is_nan() || value.is_infinite() {
            Err(Error::from(ErrorKind::InfinityOrNan))
        } else {
            let mut mat = vec![];
            for y in 0..self.rows {
                let raw = if y == i {
                    let mut raw = vec![];
                    for x in 0..self.cols {
                        let element = self.mat[y][x] * value;
                        if element.is_nan() || element.is_infinite() {
                            return Err(Error::from(ErrorKind::InfinityOrNan));
                        }
                        raw.push(element);
                    }
                    raw
                } else {
                    self.mat[y].clone()
                };
                mat.push(raw);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }

    fn row_addition_transformation(self, i: usize, j: usize, value: f64) -> Result<Self, Error> {
        if i >= self.rows || j >= self.rows {
            Err(Error::from(ErrorKind::IndexOutOfBounds))
        } else if value.is_nan() || value.is_infinite() {
            Err(Error::from(ErrorKind::InfinityOrNan))
        } else {
            let mut mat = vec![];
            for y in 0..self.rows {
                let raw = if y == j {
                    let mut raw = vec![];
                    for x in 0..self.cols {
                        let element = self.mat[y][x] + self.mat[i][x] * value;
                        if element.is_nan() || element.is_infinite() {
                            return Err(Error::from(ErrorKind::InfinityOrNan));
                        }
                        raw.push(element);
                    }
                    raw
                } else {
                    self.mat[y].clone()
                };
                mat.push(raw);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }
}

impl ops::Add for &Matrix {
    type Output = Result<Matrix, Error>;

    fn add(self, other: Self) -> Result<Matrix, Error> {
        if self.cols != other.cols || self.rows != other.rows {
            Err(Error::from(ErrorKind::MissmMatchSizeOfMatrix))
        } else {
            let mut mat: Vec<Vec<f64>> = Vec::new();
            for y in 0..self.rows {
                let mut temp: Vec<f64> = Vec::new();
                for x in 0..self.cols {
                    let element = self.mat[y][x] + other.mat[y][x];
                    if element.is_nan() || element.is_infinite() {
                        return Err(Error::from(ErrorKind::InfinityOrNan));
                    }
                    temp.push(element);
                }
                mat.push(temp);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }
}

impl ops::Sub for &Matrix {
    type Output = Result<Matrix, Error>;

    fn sub(self, other: Self) -> Result<Matrix, Error> {
        if self.cols != other.cols || self.rows != other.rows {
            Err(Error::from(ErrorKind::MissmMatchSizeOfMatrix))
        } else {
            let mut mat: Vec<Vec<f64>> = Vec::new();
            for y in 0..self.rows {
                let mut temp: Vec<f64> = Vec::new();
                for x in 0..self.cols {
                    let element = self.mat[y][x] - other.mat[y][x];
                    if element.is_nan() || element.is_infinite() {
                        return Err(Error::from(ErrorKind::InfinityOrNan));
                    }
                    temp.push(element);
                }
                mat.push(temp);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }
}

impl cmp::PartialEq for Matrix {
    fn eq(&self, other: &Self) -> bool {
        if self.cols != other.cols || self.rows != other.rows {
            false
        } else {
            for y in 0..self.rows {
                for x in 0..self.cols {
                    if ((self.mat[y][x] - other.mat[y][x]) / self.mat[y][x]).abs() >= 1e-10
                        && ((self.mat[y][x] - other.mat[y][x]) / other.mat[y][x]).abs() >= 1e-10
                    {
                        return false;
                    }
                }
            }
            true
        }
    }
}

impl ops::Mul for &Matrix {
    type Output = Result<Matrix, Error>;

    fn mul(self, other: Self) -> Result<Matrix, Error> {
        if self.cols != other.rows {
            Err(Error::from(ErrorKind::MissmMatchSizeOfMatrix))
        } else {
            let mut mat: Vec<Vec<f64>> = Vec::new();
            for y in 0..self.rows {
                let mut temp: Vec<f64> = Vec::new();
                for x in 0..other.cols {
                    let mut value = 0.0;
                    for i in 0..self.cols {
                        value += self.mat[y][i] * other.mat[i][x];
                    }
                    if value.is_nan() || value.is_infinite() {
                        return Err(Error::from(ErrorKind::InfinityOrNan));
                    }
                    temp.push(value);
                }
                mat.push(temp);
            }
            Ok(Matrix {
                cols: self.cols,
                rows: self.rows,
                mat: mat,
            })
        }
    }
}

fn num_to_th(num: usize) -> &'static str {
    if (num / 10) % 10 == 1 || num % 10 >= 4 || num % 10 == 0 {
        "th"
    } else if num % 10 == 1 {
        "st"
    } else if num % 10 == 2 {
        "nd"
    } else {
        "rd"
    }
}

#[cfg(test)]
mod matrix_test {
    use super::*;
    #[test]
    fn matrix_check_eq() {
        let a1 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 2.0], vec![3.0, 4.0]],
        };
        let a2 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 2.0], vec![3.0, 4.0]],
        };
        let b1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 0.0, 2.0],
                vec![3.0, 4.0, -1.0],
                vec![2.0, 2.0, -4.0],
            ],
        };
        let b2 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 0.0, 2.0],
                vec![3.0, 4.0, -1.0],
                vec![2.0, 2.0, -4.0],
            ],
        };
        let c1 = Matrix {
            cols: 4,
            rows: 2,
            mat: vec![vec![1.0, 9.0, 9.0, 3.0], vec![0.0, 2.0, 2.0, 6.0]],
        };
        let c2 = Matrix {
            cols: 4,
            rows: 2,
            mat: vec![vec![1.0, 9.0, 9.0, 3.0], vec![0.0, 2.0, 2.0, 6.0]],
        };
        assert!(a1 == a2);
        assert!(b1 == b2);
        assert!(c1 == c2);
        assert!(a1 != b1);
        assert!(b1 != c1);
        assert!(c1 != a1);
    }

    #[test]
    fn matrix_check_add() {
        let a1 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 2.0], vec![3.0, 4.0]],
        };
        let a2 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![-1.0, 0.0], vec![9.0, -2.0]],
        };
        let a3 = (&a1 + &a2).unwrap();
        let a4 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![0.0, 2.0], vec![12.0, 2.0]],
        };
        assert!(a3 == a4);
    }

    #[test]
    fn matrix_check_times() {
        let a1 = Matrix {
            cols: 3,
            rows: 2,
            mat: vec![vec![1.0, 2.0, 0.0], vec![4.0, -1.0, 3.0]],
        };
        let a2 = a1.times(3.0).unwrap();
        let a3 = Matrix {
            cols: 3,
            rows: 2,
            mat: vec![vec![3.0, 6.0, 0.0], vec![12.0, -3.0, 9.0]],
        };
        assert!(a2 == a3);
    }

    #[test]
    fn matrix_check_mul() {
        let a1 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 3.0], vec![-3.0, 4.0]],
        };
        let a2 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 9.0], vec![9.0, 3.0]],
        };
        let a3 = (&a1 * &a2).unwrap();
        let a4 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![28.0, 18.0], vec![33.0, -15.0]],
        };
        let b1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 0.0, 1.0],
                vec![-2.0, 2.0, 0.0],
                vec![-1.0, -2.0, 0.0],
            ],
        };
        let b2 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![3.0, 3.0, 0.0],
                vec![2.0, -1.0, 0.0],
                vec![0.0, 1.0, -2.0],
            ],
        };
        let b3 = (&b1 * &b2).unwrap();
        let b4 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![3.0, 4.0, -2.0],
                vec![-2.0, -8.0, 0.0],
                vec![-7.0, -1.0, 0.0],
            ],
        };
        let c1 = Matrix {
            cols: 2,
            rows: 3,
            mat: vec![vec![1.0, 0.0], vec![4.0, 1.0], vec![-1.0, 2.0]],
        };
        let c2 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 3.0], vec![2.0, -1.0]],
        };
        let c3 = (&c1 * &c2).unwrap();
        let c4 = Matrix {
            cols: 2,
            rows: 3,
            mat: vec![vec![1.0, 3.0], vec![6.0, 11.0], vec![3.0, -5.0]],
        };
        let d1 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![1.0, 2.0], vec![0.0, 3.0]],
        };
        let d2 = Matrix {
            cols: 2,
            rows: 2,
            mat: vec![vec![2.0, 0.0], vec![4.0, 1.0]],
        };
        let d3 = (&d1 * &d2).unwrap();
        let d4 = (&d2 * &d1).unwrap();
        assert!(a3 == a4);
        assert!(b3 == b4);
        assert!(c3 == c4);
        assert!(d3 != d4);
    }

    #[test]
    fn matrix_check_serch_in_rank() {
        let m1 = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![1.0, 2.0, 3.0, 4.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 6.0, 7.0],
                vec![0.0, 8.0, 9.0, 10.0],
            ],
        };
        let m2 = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![1.0, 2.0, 3.0, 4.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 6.0, 7.0],
                vec![0.0, 0.0, 9.0, 10.0],
            ],
        };
        let m3 = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![1.0, 2.0, 3.0, 4.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 0.0, 0.0],
            ],
        };
        assert!(m1.my_special_search_in_rank(1).unwrap() == (1, 3));
        assert!(m2.my_special_search_in_rank(1).unwrap() == (2, 2));
        assert!(m3.my_special_search_in_rank(1) == None);
    }

    #[test]
    fn matrix_check_serch() {
        let m1 = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![1.0, 2.0, 3.0, 4.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 6.0, 7.0],
                vec![0.0, 8.0, 9.0, 10.0],
            ],
        };
        let m2 = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![1.0, 2.0, 3.0, 4.0],
                vec![0.0, 0.0, 0.0, 0.0],
                vec![0.0, 0.0, 6.0, 7.0],
                vec![0.0, 0.0, 9.0, 10.0],
            ],
        };
        assert!(m1.my_special_search(1).unwrap() == 3);
        assert!(m2.my_special_search(1) == None);
    }

    #[test]
    fn matrix_check_row_switch_trans() {
        let m1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![7.0, 8.0, 9.0],
            ],
        };
        let m2 = m1.row_switching_transformation(0, 1).unwrap();
        let m3 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![4.0, 5.0, 6.0],
                vec![1.0, 2.0, 3.0],
                vec![7.0, 8.0, 9.0],
            ],
        };
        assert!(m2 == m3);
    }

    #[test]
    fn matrix_check_row_multi_trans() {
        let m1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![7.0, 8.0, 9.0],
            ],
        };
        let m2 = m1.row_multiplying_transformation(2, 2.0).unwrap();
        let m3 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![14.0, 16.0, 18.0],
            ],
        };
        assert!(m2 == m3);
    }

    #[test]
    fn matrix_check_row_add_trans() {
        let m1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![7.0, 8.0, 9.0],
            ],
        };
        let m2 = m1.row_addition_transformation(0, 2, 2.0).unwrap();
        let m3 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![9.0, 12.0, 15.0],
            ],
        };
        assert!(m2 == m3);
    }

    #[test]
    fn matrix_check_trans() {
        let m1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 3.0],
                vec![4.0, 5.0, 6.0],
                vec![7.0, 8.0, 9.0],
            ],
        };
        let m2 = m1.trans();
        let m3 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 4.0, 7.0],
                vec![2.0, 5.0, 8.0],
                vec![3.0, 6.0, 9.0],
            ],
        };
        assert!(m2 == m3);
    }

    #[test]
    fn matrix_check_rust() {
        let m1 = Matrix {
            cols: 2,
            rows: 3,
            mat: vec![vec![-1.0, 2.0], vec![3.0, 1.0], vec![3.0, 2.0]],
        };
        let m2 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 3.0, 0.0],
                vec![1.0, 2.0, 1.0],
                vec![-1.0, 1.0, -4.0],
            ],
        };
        let m3 = Matrix {
            cols: 4,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, 0.0, 3.0],
                vec![2.0, 5.0, -1.0, 5.0],
                vec![0.0, -3.0, 3.0, 7.0],
            ],
        };
        assert!(m1.rank() == 2);
        assert!(m2.rank() == 2);
        assert!(m3.rank() == 3);
    }

    #[test]
    fn matrix_check_inverse() {
        let m1 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0, 2.0, -1.0],
                vec![-1.0, -1.0, 2.0],
                vec![2.0, -1.0, 1.0],
            ],
        };
        let m2 = Matrix {
            cols: 3,
            rows: 3,
            mat: vec![
                vec![1.0/8.0, -1.0/8.0, 3.0/8.0],
                vec![5.0/8.0, 3.0/8.0, -1.0/8.0],
                vec![3.0/8.0, 5.0/8.0, 1.0/8.0],
            ],
        };
        m1.inverse().unwrap().print();
        m2.print();
        assert!(m1.inverse().unwrap()==m2);
    }

    #[test]
    fn matrix_check_det() {
        let m = Matrix {
            cols: 4,
            rows: 4,
            mat: vec![
                vec![2.0, 3.0, -2.0, 4.0],
                vec![3.0, -2.0, 1.0, 2.0],
                vec![3.0, 2.0, 3.0, 4.0],
                vec![6.0, -5.0, 6.0, 3.0],
            ]
        };
        let a=m.det().unwrap();
        assert!(((a-2.0)/a).abs() < 1e-10 && ((a-2.0)/2.0).abs() < 1e-10);
    }
}
