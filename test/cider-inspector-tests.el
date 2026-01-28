;;; cider-inspectors-tests.el  -*- lexical-binding: t; -*-

;; Copyright © 2012-2026 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar@batsov.dev>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file is part of CIDER

;;; Code:

(require 'buttercup)
(require 'cider-inspector)

;; Please, for each `describe', ensure there's an `it' block, so that its execution is visible in CI.

;; A real-world example of a value that can be passed to `cider-inspector-render*':
(defvar cider-inpector-tests--example-elements
  '("Class" ": "
    (:value "java.lang.Class" 0)
    (:newline)
    (:newline)
    "--- Interfaces:"
    (:newline)
    "  "
    (:value "clojure.lang.Counted" 1)
    (:newline)
    "  "
    (:value "clojure.lang.IPersistentList" 2)
    (:newline)
    "  "
    (:value "clojure.lang.IReduce" 3)
    (:newline)
    "  "
    (:value "java.util.List" 4)
    (:newline)
    (:newline)
    "--- Constructors:"
    (:newline)
    "  "
    (:value "public clojure.lang.PersistentList(java.lang.Object)" 5)
    (:newline)
    (:newline)
    "--- Fields:"
    (:newline)
    "  "
    (:value "public static final clojure.lang.PersistentList$EmptyList clojure.lang.PersistentList.EMPTY" 6)
    (:newline)
    "  "
    (:value "public static clojure.lang.IFn clojure.lang.PersistentList.creator" 7)
    (:newline)
    (:newline)
    "--- Methods:"
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.add(java.lang.Object)" 8)
    (:newline)
    "  "
    (:value "public void clojure.lang.ASeq.add(int,java.lang.Object)" 9)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.addAll(int,java.util.Collection)" 10)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.addAll(java.util.Collection)" 11)
    (:newline)
    "  "
    (:value "public void clojure.lang.ASeq.clear()" 12)
    (:newline)
    "  "
    (:value "public clojure.lang.IPersistentCollection clojure.lang.PersistentList.cons(java.lang.Object)" 13)
    (:newline)
    "  "
    (:value "public clojure.lang.ISeq clojure.lang.PersistentList.cons(java.lang.Object)" 14)
    (:newline)
    "  "
    (:value "public clojure.lang.PersistentList clojure.lang.PersistentList.cons(java.lang.Object)" 15)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.contains(java.lang.Object)" 16)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.containsAll(java.util.Collection)" 17)
    (:newline)
    "  "
    (:value "public int clojure.lang.PersistentList.count()" 18)
    (:newline)
    "  "
    (:value "public static clojure.lang.IPersistentList clojure.lang.PersistentList.create(java.util.List)" 19)
    (:newline)
    "  "
    (:value "public clojure.lang.IPersistentCollection clojure.lang.PersistentList.empty()" 20)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.equals(java.lang.Object)" 21)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.equiv(java.lang.Object)" 22)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.PersistentList.first()" 23)
    (:newline)
    "  "
    (:value "public default void java.lang.Iterable.forEach(java.util.function.Consumer)" 24)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.ASeq.get(int)" 25)
    (:newline)
    "  "
    (:value "public final native java.lang.Class java.lang.Object.getClass()" 26)
    (:newline)
    "  "
    (:value "public int clojure.lang.ASeq.hashCode()" 27)
    (:newline)
    "  "
    (:value "public int clojure.lang.ASeq.hasheq()" 28)
    (:newline)
    "  "
    (:value "public int clojure.lang.ASeq.indexOf(java.lang.Object)" 29)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.isEmpty()" 30)
    (:newline)
    "  "
    (:value "public java.util.Iterator clojure.lang.ASeq.iterator()" 31)
    (:newline)
    "  "
    (:value "public int clojure.lang.ASeq.lastIndexOf(java.lang.Object)" 32)
    (:newline)
    "  "
    (:value "public java.util.ListIterator clojure.lang.ASeq.listIterator()" 33)
    (:newline)
    "  "
    (:value "public java.util.ListIterator clojure.lang.ASeq.listIterator(int)" 34)
    (:newline)
    "  "
    (:value "public final clojure.lang.IPersistentMap clojure.lang.Obj.meta()" 35)
    (:newline)
    "  "
    (:value "public clojure.lang.ISeq clojure.lang.ASeq.more()" 36)
    (:newline)
    "  "
    (:value "public clojure.lang.ISeq clojure.lang.PersistentList.next()" 37)
    (:newline)
    "  "
    (:value "public final native void java.lang.Object.notify()" 38)
    (:newline)
    "  "
    (:value "public final native void java.lang.Object.notifyAll()" 39)
    (:newline)
    "  "
    (:value "public default java.util.stream.Stream java.util.Collection.parallelStream()" 40)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.PersistentList.peek()" 41)
    (:newline)
    "  "
    (:value "public clojure.lang.IPersistentList clojure.lang.PersistentList.pop()" 42)
    (:newline)
    "  "
    (:value "public clojure.lang.IPersistentStack clojure.lang.PersistentList.pop()" 43)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.PersistentList.reduce(clojure.lang.IFn)" 44)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.PersistentList.reduce(clojure.lang.IFn,java.lang.Object)" 45)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.remove(java.lang.Object)" 46)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.ASeq.remove(int)" 47)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.removeAll(java.util.Collection)" 48)
    (:newline)
    "  "
    (:value "public default boolean java.util.Collection.removeIf(java.util.function.Predicate)" 49)
    (:newline)
    "  "
    (:value "public default void java.util.List.replaceAll(java.util.function.UnaryOperator)" 50)
    (:newline)
    "  "
    (:value "public boolean clojure.lang.ASeq.retainAll(java.util.Collection)" 51)
    (:newline)
    "  "
    (:value "public final clojure.lang.ISeq clojure.lang.ASeq.seq()" 52)
    (:newline)
    "  "
    (:value "public java.lang.Object clojure.lang.ASeq.set(int,java.lang.Object)" 53)
    (:newline)
    "  "
    (:value "public int clojure.lang.ASeq.size()" 54)
    (:newline)
    "  "
    (:value "public default void java.util.List.sort(java.util.Comparator)" 55)
    (:newline)
    "  "
    (:value "public default java.util.Spliterator java.util.List.spliterator()" 56)
    (:newline)
    "  "
    (:value "public default java.util.stream.Stream java.util.Collection.stream()" 57)
    (:newline)
    "  "
    (:value "public java.util.List clojure.lang.ASeq.subList(int,int)" 58)
    (:newline)
    "  "
    (:value "public default java.lang.Object[] java.util.Collection.toArray(java.util.function.IntFunction)" 59)
    (:newline)
    "  "
    (:value "public java.lang.Object[] clojure.lang.ASeq.toArray()" 60)
    (:newline)
    "  "
    (:value "public java.lang.Object[] clojure.lang.ASeq.toArray(java.lang.Object[])" 61)
    (:newline)
    "  "
    (:value "public java.lang.String clojure.lang.ASeq.toString()" 62)
    (:newline)
    "  "
    (:value "public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException" 63)
    (:newline)
    "  "
    (:value "public final void java.lang.Object.wait() throws java.lang.InterruptedException" 64)
    (:newline)
    "  "
    (:value "public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException" 65)
    (:newline)
    "  "
    (:value "public clojure.lang.IObj clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)" 66)
    (:newline)
    "  "
    (:value "public clojure.lang.Obj clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)" 67)
    (:newline)
    "  "
    (:value "public clojure.lang.PersistentList clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)" 68)
    (:newline)
    (:newline)
    "--- Datafy:"
    (:newline)
    "  "
    (:value ":bases" 69)
    " = "
    (:value "#{ clojure.lang.IReduce clojure.lang.Counted clojure.lang.ASeq java.util.List clojure.lang.IPersistentList }" 70)
    (:newline)
    "  "
    (:value ":flags" 71)
    " = "
    (:value "#{ :public }" 72)
    (:newline)
    "  "
    (:value ":members" 73)
    " = "
    (:value "{ EMPTY [ { :name EMPTY, :type clojure.lang.PersistentList$EmptyList, :declaring-class clojure.lang.PersistentList, :flags #{ :public :static :final } } ], _count [ { :name _count, :type int, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], _first [ { :name _first, :type java.lang.Object, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], _rest [ { :name _rest, :type clojure.lang.IPersistentList, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], clojure.lang.PersistentList [ { :name clojure.lang.PersistentList, :declaring-class clojure.lang.PersistentList, :parameter-types [ clojure.lang.IPersistentMap java.lang.Object clojure.lang.IPersistentList int ], :exception-types [], :flags #{} } { :name clojure.lang.PersistentList, :declaring-class clojure.lang.PersistentList, :parameter-types [ java.lang.Object ], :exception-types [], :flags #{ :public } } ], ... }" 74)
    (:newline)
    "  "
    (:value ":name" 75)
    " = "
    (:value "clojure.lang.PersistentList" 76)
    (:newline)
    (:newline)
    "--- Path:"
    (:newline)
    "  " "class"))

(describe "cider-inspector-render*"
  (it "Produces a well-known string without errors"
    (expect
     (with-temp-buffer
       (cider-inspector-render* cider-inpector-tests--example-elements)
       (buffer-string))
     :to-equal "Class: java.lang.Class

--- Interfaces:
  clojure.lang.Counted
  clojure.lang.IPersistentList
  clojure.lang.IReduce
  java.util.List

--- Constructors:
  public clojure.lang.PersistentList(java.lang.Object)

--- Fields:
  public static final clojure.lang.PersistentList$EmptyList clojure.lang.PersistentList.EMPTY
  public static clojure.lang.IFn clojure.lang.PersistentList.creator

--- Methods:
  public boolean clojure.lang.ASeq.add(java.lang.Object)
  public void clojure.lang.ASeq.add(int,java.lang.Object)
  public boolean clojure.lang.ASeq.addAll(int,java.util.Collection)
  public boolean clojure.lang.ASeq.addAll(java.util.Collection)
  public void clojure.lang.ASeq.clear()
  public clojure.lang.IPersistentCollection clojure.lang.PersistentList.cons(java.lang.Object)
  public clojure.lang.ISeq clojure.lang.PersistentList.cons(java.lang.Object)
  public clojure.lang.PersistentList clojure.lang.PersistentList.cons(java.lang.Object)
  public boolean clojure.lang.ASeq.contains(java.lang.Object)
  public boolean clojure.lang.ASeq.containsAll(java.util.Collection)
  public int clojure.lang.PersistentList.count()
  public static clojure.lang.IPersistentList clojure.lang.PersistentList.create(java.util.List)
  public clojure.lang.IPersistentCollection clojure.lang.PersistentList.empty()
  public boolean clojure.lang.ASeq.equals(java.lang.Object)
  public boolean clojure.lang.ASeq.equiv(java.lang.Object)
  public java.lang.Object clojure.lang.PersistentList.first()
  public default void java.lang.Iterable.forEach(java.util.function.Consumer)
  public java.lang.Object clojure.lang.ASeq.get(int)
  public final native java.lang.Class java.lang.Object.getClass()
  public int clojure.lang.ASeq.hashCode()
  public int clojure.lang.ASeq.hasheq()
  public int clojure.lang.ASeq.indexOf(java.lang.Object)
  public boolean clojure.lang.ASeq.isEmpty()
  public java.util.Iterator clojure.lang.ASeq.iterator()
  public int clojure.lang.ASeq.lastIndexOf(java.lang.Object)
  public java.util.ListIterator clojure.lang.ASeq.listIterator()
  public java.util.ListIterator clojure.lang.ASeq.listIterator(int)
  public final clojure.lang.IPersistentMap clojure.lang.Obj.meta()
  public clojure.lang.ISeq clojure.lang.ASeq.more()
  public clojure.lang.ISeq clojure.lang.PersistentList.next()
  public final native void java.lang.Object.notify()
  public final native void java.lang.Object.notifyAll()
  public default java.util.stream.Stream java.util.Collection.parallelStream()
  public java.lang.Object clojure.lang.PersistentList.peek()
  public clojure.lang.IPersistentList clojure.lang.PersistentList.pop()
  public clojure.lang.IPersistentStack clojure.lang.PersistentList.pop()
  public java.lang.Object clojure.lang.PersistentList.reduce(clojure.lang.IFn)
  public java.lang.Object clojure.lang.PersistentList.reduce(clojure.lang.IFn,java.lang.Object)
  public boolean clojure.lang.ASeq.remove(java.lang.Object)
  public java.lang.Object clojure.lang.ASeq.remove(int)
  public boolean clojure.lang.ASeq.removeAll(java.util.Collection)
  public default boolean java.util.Collection.removeIf(java.util.function.Predicate)
  public default void java.util.List.replaceAll(java.util.function.UnaryOperator)
  public boolean clojure.lang.ASeq.retainAll(java.util.Collection)
  public final clojure.lang.ISeq clojure.lang.ASeq.seq()
  public java.lang.Object clojure.lang.ASeq.set(int,java.lang.Object)
  public int clojure.lang.ASeq.size()
  public default void java.util.List.sort(java.util.Comparator)
  public default java.util.Spliterator java.util.List.spliterator()
  public default java.util.stream.Stream java.util.Collection.stream()
  public java.util.List clojure.lang.ASeq.subList(int,int)
  public default java.lang.Object[] java.util.Collection.toArray(java.util.function.IntFunction)
  public java.lang.Object[] clojure.lang.ASeq.toArray()
  public java.lang.Object[] clojure.lang.ASeq.toArray(java.lang.Object[])
  public java.lang.String clojure.lang.ASeq.toString()
  public final native void java.lang.Object.wait(long) throws java.lang.InterruptedException
  public final void java.lang.Object.wait() throws java.lang.InterruptedException
  public final void java.lang.Object.wait(long,int) throws java.lang.InterruptedException
  public clojure.lang.IObj clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)
  public clojure.lang.Obj clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)
  public clojure.lang.PersistentList clojure.lang.PersistentList.withMeta(clojure.lang.IPersistentMap)

--- Datafy:
  :bases = #{ clojure.lang.IReduce clojure.lang.Counted clojure.lang.ASeq java.util.List clojure.lang.IPersistentList }
  :flags = #{ :public }
  :members = { EMPTY [ { :name EMPTY, :type clojure.lang.PersistentList$EmptyList, :declaring-class clojure.lang.PersistentList, :flags #{ :public :static :final } } ], _count [ { :name _count, :type int, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], _first [ { :name _first, :type java.lang.Object, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], _rest [ { :name _rest, :type clojure.lang.IPersistentList, :declaring-class clojure.lang.PersistentList, :flags #{ :private :final } } ], clojure.lang.PersistentList [ { :name clojure.lang.PersistentList, :declaring-class clojure.lang.PersistentList, :parameter-types [ clojure.lang.IPersistentMap java.lang.Object clojure.lang.IPersistentList int ], :exception-types [], :flags #{} } { :name clojure.lang.PersistentList, :declaring-class clojure.lang.PersistentList, :parameter-types [ java.lang.Object ], :exception-types [], :flags #{ :public } } ], ... }
  :name = clojure.lang.PersistentList

--- Path:
  class")))
