/**
 * GroupByStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: jarodree
 * Person#: 50269702
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}
import collection.mutable.ArrayBuffer
import util.control.Breaks._

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var iterable_entries: ArrayBuffer[TaxEntry] = new ArrayBuffer[TaxEntry]()
  private var groupingAttribute = "STREET"
  private var numStored = 0
  private var group_val_idx: ArrayBuffer[String] = new ArrayBuffer[String]()

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {
    if (group_val_idx.indexOf(taxEntry.infoMap(groupingAttribute)) == -1 || group_val_idx.isEmpty) {
      val group: DNode[TaxEntry] = new DNode[TaxEntry](value = taxEntry, next = null, prev = null)
      group_val_idx += taxEntry.infoMap(groupingAttribute)
      groupings += group
      iterable_entries += taxEntry
      numStored += 1
    } else {
      val next_group = groupings(group_val_idx.indexOf(taxEntry.infoMap(groupingAttribute)))
      val group: DNode[TaxEntry] = new DNode[TaxEntry](value = taxEntry, next = next_group, prev = null)
      next_group.prev = group
      groupings(group_val_idx.indexOf(taxEntry.infoMap(groupingAttribute))) = group
      iterable_entries += taxEntry
      numStored += 1
    }
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = ???

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    private var current = iterable_entries.head
    override def hasNext: Boolean = {
      if(current != null){
        true
      }else {
        false
      }
    }

    override def next(): TaxEntry = {
      val prev = current
      if(iterable_entries.indexOf(current) + 1 >= iterable_entries.length){
        current = null
      } else{
        current = iterable_entries(iterable_entries.indexOf(current) + 1)
      }
      prev
    }
  }
  
  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    private var current = groupings(group_val_idx.indexOf(value))
    override def hasNext: Boolean = {
      println("ITERATING")
      current.next != null
    }

    override def next(): TaxEntry = {
      val prev = current
      current = current.next
      prev.value
    }
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
