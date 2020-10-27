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
  val operatorActor = context.spawn(Operator(), "operator")

  override def onMessage(msg: Trader.Command): Behavior[Trader.Command] =
    msg match {
      case Operator.Bought(price) =>
        lastOp = Some(Trader.Operations.Buy)
        lastPrice = Some(price)
        Behaviors.same
      case Operator.Sold(price) =>
        lastOp = Some(Trader.Operations.Sell)
        lastPrice = Some(price)
        Behaviors.same
      case CoinPrice(price, replyTo) =>
        lastOp match {
          case Some(value) =>
            value match {
              case Buy =>
                operatorActor ! Operator.BuyOperation(price,
                                                    lastPrice.get,
                                                    this.context.self)
              case Sell =>
                operatorActor ! Operator.SellOperation(price,
                                                       lastPrice.get,
                                                       this.context.self)
            }
          case None =>
            operatorActor ! Operator.BuyOperation(price, 0, this.context.self)
        }
        this
      case Operator.NoOp => Behaviors.same

    }
}
