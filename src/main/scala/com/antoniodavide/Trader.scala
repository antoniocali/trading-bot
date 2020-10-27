package com.antoniodavide

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import com.antoniodavide.Trader.CoinPrice

object Trader {

  def apply(): Behavior[Trader.Command] =
    Behaviors.setup(context => new Trader(context))
  trait Command

  final case class CoinPrice(price: Double, replyTo: ActorRef[Command])
      extends Command
  object Operations extends Enumeration {
    type Operation = Value
    val Buy, Sell = Value
  }
}

class Trader(context: ActorContext[Trader.Command])
    extends AbstractBehavior[Trader.Command](context = context) {
  import Trader.Operations._
  var lastOp: Option[Operation] = None
  var lastPrice: Option[Double] = None
  val buyActor = context.spawn(BuyOperator(), "buy-operator")
  val sellActor = context.spawn(SellOperator(), name = "sell-operator")

  override def onMessage(msg: Trader.Command): Behavior[Trader.Command] =
    msg match {
      case BuyOperator.Bought(price) =>
        lastOp = Some(Trader.Operations.Buy)
        lastPrice = Some(price)
        Behaviors.same
      case SellOperator.Sold(price) =>
        lastOp = Some(Trader.Operations.Sell)
        lastPrice = Some(price)
        Behaviors.same
      case CoinPrice(price, replyTo) =>
        lastOp match {
          case Some(value) =>
            value match {
              case Buy =>
                buyActor ! BuyOperator.BuyOperation(price,
                                                    lastPrice.get,
                                                    this.context.self)
              case Sell =>
                sellActor ! SellOperator.SellOperation(price,
                                                       lastPrice.get,
                                                       this.context.self)
            }
          case None =>
            buyActor ! BuyOperator.BuyOperation(price, 0, this.context.self)
        }
        this
      case BuyOperator.NoOp | SellOperator.NoOp => Behaviors.same

    }
}
