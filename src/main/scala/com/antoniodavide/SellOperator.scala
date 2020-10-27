package com.antoniodavide

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
object SellOperator {
  trait Command

  final case object NoOp extends Command with Trader.Command
  final case class SellOperation(price: Double,
                                 lastPrice: Double,
                                 replyTo: ActorRef[Trader.Command])
      extends Command
  final case class Sold(price: Double) extends Command with Trader.Command
  def apply(): Behavior[Command] =
    Behaviors.setup(context => new SellOperator(context))
}

class SellOperator(context: ActorContext[SellOperator.Command])
    extends AbstractBehavior(context = context) {
  override def onMessage(
      msg: SellOperator.Command): Behavior[SellOperator.Command] = msg match {
    case SellOperator.SellOperation(price, lastPrice, replyTo) =>
      if (price > lastPrice) replyTo ! SellOperator.NoOp
      else replyTo ! SellOperator.Sold(price)
      this
  }
}
