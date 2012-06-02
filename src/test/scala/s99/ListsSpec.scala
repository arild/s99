package s99
import org.specs2.mutable._

class ListsSpec extends Specification with ListsSolutions {

  "Find the last element of a list" >>
  { last(List(1, 1, 2, 3, 5, 8)) === 8 }

  "Find the last but one element of a list" >>
  { penultimate(List(1, 1, 2, 3, 5, 8)) === 5 }

  "Find the nth element of a list. By convention, the first element in the list is element 0" >>
  { nth(2, List(1, 1, 2, 3, 5, 8)) === 2 }

  "Find the number of elements of a list" >>
  { length(List(1, 1, 2, 3, 5, 8)) === 6 }

  "Reverse a list" >>
  { reverse(List(1, 1, 2, 3, 5, 8)) === List(8, 5, 3, 2, 1, 1) }

  "Find out whether a list is a palindrome" >>
  { isPalindrome(List(1, 2, 3, 2, 1)) must beTrue }

  "Flatten a nested list structure" >>
  { flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8) }

  """ Eliminate consecutive duplicates of list elements
  If a list contains repeated elements they
  should be replaced with a single copy of the element. The order of the elements should not be changed""" >>
  { compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e) }

  """ Pack consecutive duplicates of list elements into sublists
  If a list contains repeated elements they should be placed in separate sublists """ >>
  { pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) }

  """ Run-length encoding of a list
  Use the result of problem P09 to implement the so-called run-length encoding data compression method.
  Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number
  of duplicates of the element E""" >>
  { encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) }

  """ Modified run-length encoding
  Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the
  result list. Only elements with duplicates are transferred as (N, E) terms""" >>
   { encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)) }

  """ Decode a run-length encoded list
  Given a run-length code list generated as specified in problem P10, construct its uncompressed version""" >>
  { decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) ===
      List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e) }

  """ Run-length encoding of a list (direct solution)
  Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've
  written (like P09's pack); do all the work directly""" >>
  { encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) ===
      List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) }

  "Duplicate the elements of a list" >>
  { duplicate(List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd) }

  "Duplicate the elements of a list a given number of times" >>
  { duplicateN(3, List('a, 'b, 'c, 'c, 'd)) === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd) }

  "Drop every Nth element from a list" >>
  { drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k) }

  "Split a list into two parts.  The length of the first part is given. Use a Tuple for your result" >>
  { split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) }

  """ Extract a slice from a list
  Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to
  but not including the Kth element of the original list. Start counting the elements with 0""" >>
  { slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g) }

  "Rotate a list N places to the left" >>
  { rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) }

  """ Remove the Kth element from a list
  Return the list and the removed element in a Tuple. Elements are numbered from 0""" >>
  { removeAt(1, List('a, 'b, 'c, 'd)) === (List('a, 'c, 'd),'b) }

  "Insert an element at a given position into a list" >>
  { insertAt('new, 1, List('a, 'b, 'c, 'd)) === List('a, 'new, 'b, 'c, 'd) }

  "Create a list containing all integers within a given range" >>
  { range(4, 9) === List(4, 5, 6, 7, 8, 9) }

  "Extract a given number of randomly selected elements from a list. Hint: Use the solution to problem P20" >>
  { val selected = randomSelect(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h))
    selected.size === 3
    selected.distinct.size === selected.size }

  "Lotto: Draw N different random numbers from the set 1..M" >>
  { val selected = lotto(6, 49)
    selected.size === 6
    ((_: Int) must be_<=(49)).forall(selected) }

  "Generate a random permutation of the elements of a list. Hint: Use the solution of problem P23" >>
  { val permute = randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
    permute.size === 6
    permute.distinct.size === permute.size }

  """ Generate the combinations of K distinct objects chosen from the N elements of a list
  In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are
  C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this
  result may be great But we want to really generate all the possibilities""" >>
  { combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) must contain(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e)) }

  """ Group the elements of a set into disjoint subsets
   a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons?
      Write a function that generates all the possibilities.
   b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate
      will return a list of groups.

   Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution
   as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and
   ((Carla, David), (Aldo, Beat), ...).

  You may find more about this combinatorial problem in a good book on discrete mathematics under the term
  "multinomial coefficients" """ >>
  {
    groups(List(1, 1), List("Aldo", "Beat")) must contain (
      List(List("Aldo"), List("Beat")))

    group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) must contain (
      List(List("Aldo", "Beat"), List("Carla", "David", "Evi"), List("Flip", "Gary", "Hugo", "Ida")))

    groups(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) must contain(
      List(List("Aldo", "Beat"), List("Carla", "David"), List("Evi", "Flip", "Gary", "Hugo", "Ida"))) }

  """ Sorting a list of lists according to length of sublists
    a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements
       of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
    b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is
       to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly,
       lists with rare lengths are placed, others with a more frequent length come later""" >>
  { lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) ===
      List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))

     // Note that in this example, the first two lists in the result have length 4 and 1 and both lengths appear just once.
     // The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have
     // length 2. This is the most frequent length.
     lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) ===
       List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n)) }
}