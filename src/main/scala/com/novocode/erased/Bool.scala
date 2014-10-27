package com.novocode.erased

/** Type-level boolean. */
sealed trait Bool {
  type IfElse[B, T <: B, F <: B] <: B
  type && [B <: Bool] <: Bool
  type || [B <: Bool] <: Bool
  def ifElse[B, T <: B, F <: B](t: =>T, f: =>F): IfElse[B, T, F]
  def && [B <: Bool](b: B): &&[B]
  def || [B <: Bool](b: B): ||[B]
}

object True extends Bool {
  type IfElse[B, T <: B, F <: B] = T
  type && [B <: Bool] = B
  type || [B <: Bool] = True.type
  def ifElse[B, T <: B, F <: B](t: =>T, f: =>F): IfElse[B, T, F] = t
  def && [B <: Bool](b: B) = b
  def || [B <: Bool](b: B) = True
}

object False extends Bool {
  type IfElse[B, T <: B, F <: B] = F
  type && [B <: Bool] = False.type
  type || [B <: Bool] = B
  def ifElse[B, T <: B, F <: B](t: =>T, f: =>F): IfElse[B, T, F] = f
  def && [B <: Bool](b: B) = False
  def || [B <: Bool](b: B) = b
}
