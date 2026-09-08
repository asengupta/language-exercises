#eval 1+2

def sum(n1: Nat) (n2: Nat) : Nat := n1 + n2
def add2 := sum 2
#eval add2 5

structure Human where
  name: String
  age: Nat
  height: Float
deriving Repr

def me : Human := {name := "Mojo", age := 45, height := 5.6}
def kochi : Human := {name := "Kochi", age := 38, height := 5.4}
#eval me

def together (a: Human) (b: Human) : Human := ({name := String.append a.name b.name, age := a.age + b.age, height := a.height + b.height } : Human)

#eval together me kochi

def zeroAge (h: Human) : Human := {h with age := 0}

#eval zeroAge kochi

def sum_nat (lhs: Nat) (rhs: Nat): Nat :=
  match rhs with
  | Nat.zero => lhs
  | Nat.succ rhs' => sum (Nat.succ lhs) rhs'

#eval sum_nat 5 9

inductive OogaBooga where
  | Grog
  | Hofmandi

def lol (x: OogaBooga) : match x with | OogaBooga.Grog => String | OogaBooga.Hofmandi => Nat :=
match x with
  | OogaBooga.Grog => "ABCD"
  | OogaBooga.Hofmandi => 3000

#eval lol OogaBooga.Grog

structure ArbitraryPair (T: Type) where
  lhs: T
  rhs: T
deriving Repr

def pairOfNumbers: ArbitraryPair Nat := {lhs:= 10, rhs:= 20}
#eval pairOfNumbers

def left_set_to_right (x: ArbitraryPair T): ArbitraryPair T := {x with lhs := x.rhs}
#eval left_set_to_right {lhs:= 20, rhs:= 30}

def len (T: Type) (l: List T): Nat :=
  match l with
  | List.nil => Nat.zero
  | List.cons _ tail => Nat.succ (len T tail)

#eval len Nat [1,2,3,4]

def lst (a: Type) (l: List a): Option a :=
  match l with
  | List.nil => Option.none
  | List.cons h List.nil => h
  | List.cons _ tail => lst a tail

#eval lst Nat [1,2,3,4,100]

def fndFrst (a: Type) (lst: List a): Option a :=
  match lst with
  | List.nil => Option.none
  | List.cons h _ => Option.some h

#eval fndFrst Nat [3,4,5,6]

def map (a: Type) (b: Type) (lst: List a) (predicate: (e: a) -> b): List b:=
  match lst with
  | [] => []
  | h :: tail => (predicate h) :: (map a b tail predicate)

def multiply_by_2 (n: Nat): Nat := n*2

#eval map Nat Nat [1,2,3,4,5] multiply_by_2

def findFirstMatching (a: Type) (lst: List a) (predicate: (e: a) -> Bool): Option a :=
  match lst with
  | [] => Option.none
  | h :: tail => match (predicate h) with
    | Bool.true => Option.some h
    | Bool.false => findFirstMatching a tail predicate

def even (n: Nat): Bool := n % 2 = 0

#eval findFirstMatching Nat [1,2,3,4,5] even

