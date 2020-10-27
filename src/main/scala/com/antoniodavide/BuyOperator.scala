package com.antoniodavide

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
object BuyOperator {
  trait Command
  final case class BuyOperation(price: Double,
                                lastPrice: Double,
                                replyTo: ActorRef[Trader.Command])
      extends Command
  final case object NoOp extends Command with Trader.Command
  final case class Bought(price: Double) extends Command with Trader.Command
  def apply(): Behavior[Command] =
    Behaviors.setup(context => new BuyOperator(context))
}

class BuyOperator(context: ActorContext[BuyOperator.Command])
    extends AbstractBehavior(context = context) {
  override def onMessage(
      msg: BuyOperator.Command): Behavior[BuyOperator.Command] = {
    msg match {
      case BuyOperator.BuyOperation(price, lastPrice, replyTo)
          if lastPrice < price =>
        replyTo ! BuyOperator.NoOp
      case BuyOperator.BuyOperation(price, lastPrice, replyTo) =>
        replyTo ! BuyOperator.Bought(price)
    }
    this
  }
}
