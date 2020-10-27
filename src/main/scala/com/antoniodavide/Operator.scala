package com.antoniodavide

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
object Operator {
  trait Command
  final case class BuyOperation(price: Double,
                                lastPrice: Double,
                                replyTo: ActorRef[Trader.Command])
      extends Command
  final case object NoOp extends Command with Trader.Command
  final case class Bought(price: Double) extends Command with Trader.Command
  final case class SellOperation(price: Double,
                                 lastPrice: Double,
                                 replyTo: ActorRef[Trader.Command])
    extends Command
  final case class Sold(price: Double) extends Command with Trader.Command
  def apply(): Behavior[Command] =
    Behaviors.setup(context => new Operator(context))
}

class Operator(context: ActorContext[Operator.Command])
    extends AbstractBehavior(context = context) {
  override def onMessage(
      msg: Operator.Command): Behavior[Operator.Command] = {
    msg match {
      case Operator.BuyOperation(price, lastPrice, replyTo)
          if lastPrice < price =>
        replyTo ! Operator.NoOp
      case Operator.BuyOperation(price, lastPrice, replyTo) =>
        replyTo ! Operator.Bought(price)
    }
    this
  }
}
