object problems {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(218); 
  //P01 (*) Find the last element of a list.
  def last[A](list: List[A]): A = list match {
    case l :: Nil => l
    case _ :: rest => last(rest)
    case _ => throw new NoSuchElementException()
  };System.out.println("""last: [A](list: List[A])A""");$skip(229); 

  //P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]): A = list match {
    case h :: _ :: Nil => h
    case _ :: rest => penultimate(rest)
    case _ => throw new NoSuchElementException()
  };System.out.println("""penultimate: [A](list: List[A])A""");$skip(242); 

  //P03 (*) Find the Kth element of a list.
  def nth[A](index: Int, list: List[A]): A = (index, list) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => nth(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException()
  };System.out.println("""nth: [A](index: Int, list: List[A])A""");$skip(255); 

  //P04 (*) Find the number of elements of a list.
  def length(list: List[Any]) = {
      def len(acc: Int, remaining: List[Any]): Int = remaining match {
        case Nil => acc
        case _ :: rest => len(acc + 1, rest)
      }
    len(0, list)
  };System.out.println("""length: (list: List[Any])Int""");$skip(75); 

  //P05 (*) Reverse a list.
  def reverse(list: List[Any]) = list.reverse;System.out.println("""reverse: (list: List[Any])List[Any]""");$skip(113); 

  //P06 (*) Find out whether a list is a palindrome.
  def isPalindrome(list: List[Any]) = list.reverse == list;System.out.println("""isPalindrome: (list: List[Any])Boolean""");$skip(286); 
  //P07 (**) Flatten a nested list structure.
  def flatten(list: List[Any]): List[Any] =
    if (list == Nil) Nil
    else {
      val (head :: rest) = list
      head match {
        case l: List[Any] => flatten(l) ::: flatten(rest)
        case e => e :: flatten(rest)
      }
    };System.out.println("""flatten: (list: List[Any])List[Any]""");$skip(438); 

  //P08 (**) Eliminate consecutive duplicates of list elements. If a
  //list contains repeated elements they should be replaced with a single copy of the element.
  //The order of the elements should not be changed.
  def compress(list: List[Any]): List[Any] = list match {
    case Nil => Nil
    case x :: Nil => List(x)
    case x :: y :: rest if x == y => compress(y :: rest)
    case x :: y :: rest => x :: compress(y :: rest)
  };System.out.println("""compress: (list: List[Any])List[Any]""");$skip(251); 

  def whileSatisfy(f: Any => Boolean, list: List[Any]): (List[Any], List[Any]) = list match {
    case Nil => (Nil, Nil)
    case el :: rest => if (f(el)) {
      val (e, tail) = whileSatisfy(f, rest)
      (el :: e, tail)
    } else (Nil, list)
  };System.out.println("""whileSatisfy: (f: Any => Boolean, list: List[Any])(List[Any], List[Any])""");$skip(347); 

  //P09 (**) Pack consecutive duplicates of list elements into sublists.
  //If a list contains repeated elements they should be placed in separate sublists.
  def pack(list: List[Any]): List[List[Any]] = {
    if (list.length > 0) {
      val (elements, tail) = whileSatisfy(_ == list.head, list)
      elements :: pack(tail)
    } else Nil
  };System.out.println("""pack: (list: List[Any])List[List[Any]]""");$skip(487); 

  //P10 (*) Run-length encoding of a list.
  //Use the result of problem P09 to implement the so-called
  //run-length encoding data compression method. Consecutive duplicates
  //of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
  def encode(list: List[Any]): List[(Int, Any)] =
    if (list.length > 0) {
      val (elements, tail) = whileSatisfy(_ == list.head, list)
      (elements.length, elements.head) :: encode(tail)
    } else Nil;System.out.println("""encode: (list: List[Any])List[(Int, Any)]""");$skip(422); 

  //P11 (*) Modified run-length encoding.
  //Modify the result of problem P10 in such a way that if an element
  //has no duplicates it is simply copied into the result list.
  //Only elements with duplicates are transferred as (N, E) terms.
  def encodeModified(list: List[Any]): List[Any] =
    encode(list) map (
      tuple => {
        val (count, symbol) = tuple
        if (count == 1) symbol else tuple
      });System.out.println("""encodeModified: (list: List[Any])List[Any]""");$skip(270); 

  //P12 (**) Decode a run-length encoded list.
  //Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  def decode(encoded: List[(Int, Symbol)]) = encoded.map(tuple => List.fill(tuple._1)(tuple._2)) reduce (_ ::: _);System.out.println("""decode: (encoded: List[(Int, Symbol)])List[Symbol]""");$skip(412); 

  //P13 (**) Run-length encoding of a list (direct solution).
  //Implement the so-called run-length encoding data compression method directly.
  //I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  //napisałem już jako encode bez używania pack

  //P14 (*) Duplicate the elements of a list.
  def duplicate(list: List[Any]) = list map (x => List(x, x)) reduce (_ ::: _);System.out.println("""duplicate: (list: List[Any])List[Any]""");$skip(163); 
  //P16 (**) Drop every Nth element from a list.
  def drop(count: Int, list: List[Any]) = (list grouped (count) toList) map (_ take (count - 1)) reduce (_ ::: _);System.out.println("""drop: (count: Int, list: List[Any])List[Any]""");$skip(194); 
  //P17 (*) Split a list into two parts.
  //The length of the first part is given. Use a Tuple for your result.
  def split(length: Int, list: List[Any]) = (list take length, list drop length);System.out.println("""split: (length: Int, list: List[Any])(List[Any], List[Any])""");$skip(340); 

  //P18 (**) Extract a slice from a list.
  //Given two indices, I and K, the slice is the list containing the elements
  //from and including the Ith element up to but not including the Kth element
  //of the original list. Start counting the elements with 0.
  def slice(start: Int, end: Int, list: List[Any]) = list take end drop start;System.out.println("""slice: (start: Int, end: Int, list: List[Any])List[Any]""");$skip(228); 

  //P19 (**) Rotate a list N places to the left.
  def rotate(length: Int, list: List[Any]) =
    if (length < 0)
      list.takeRight(-length) ::: list.dropRight(-length)
    else
      list.drop(length) ::: list.take(length);System.out.println("""rotate: (length: Int, list: List[Any])List[Any]""");$skip(328); 

  //P20 (*) Remove the Kth element from a list.
  //Return the list and the removed element in a Tuple. Elements are numbered from 0.
  def removeAt(index: Int, list: List[Any]) =
    if (index < list.length)
      (list.take(index) ::: list.drop(index + 1), list drop index head)
    else throw new IllegalArgumentException();System.out.println("""removeAt: (index: Int, list: List[Any])(List[Any], Any)""");$skip(189); 

  //P21 (*) Insert an element at a given position into a list.
  def insertAt[A](value: A, index: Int, list: List[A]) = list splitAt index match {case (begin,rest) => begin:::value::rest};System.out.println("""insertAt: [A](value: A, index: Int, list: List[A])List[A]""");$skip(175); 
                                                  
	//http://aperiodic.net/phil/scala/s-99/
	

  assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd));$skip(70); 
  assert(removeAt(2, List('a, 'b, 'c, 'd)) == (List('a, 'b, 'd), 'c));$skip(122); 
  assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c));$skip(123); 
  assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i));$skip(96); 
  assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g));$skip(129); 
  assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)));$skip(108); 
  assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k));$skip(94); 
  assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd));$skip(141); 
  assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e));$skip(139); 
  assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), 'b, (2, 'c), (2, 'a), 'd, (4, 'e)));$skip(141); 
  assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)));$skip(177); 
  assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)));$skip(41); 
  assert(compress(List('a)) == List('a));$skip(45); 
  assert(compress(List('a, 'a)) == List('a));$skip(113); 
  assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e));$skip(86); 
  assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8));$skip(44); 
  assert(isPalindrome(List(1, 2, 3, 2, 1)));$skip(68); 
  assert(reverse(List(1, 1, 2, 3, 5, 8)) == List(8, 5, 3, 2, 1, 1));$skip(46); 
  assert(length(List(1, 1, 2, 3, 5, 8)) == 6);$skip(46); 
  assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2);$skip(51); 
  assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5);$skip(44); 
  assert(last(List(1, 1, 2, 3, 5, 8)) == 8);$skip(16); 

  val s = "OK";System.out.println("""s  : String = """ + $show(s ))}
}
