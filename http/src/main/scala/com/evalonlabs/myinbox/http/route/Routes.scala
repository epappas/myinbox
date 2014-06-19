package com.evalonlabs.myinbox.http.route


import io.netty.channel.ChannelHandlerContext
import com.evalonlabs.net.http.{ContextSystem, HttpResponse => MyHttpResponse, RouteHandler}
import java.util
import com.evalonlabs.myinbox.http.route.call.{Outbound, Inbound, IndexCall}
import io.netty.handler.codec.http.{HttpContent, HttpRequest}

object Routes {
  def get(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => IndexCall(system)
      case index: String if path.matches("/outbound(/?)") => Outbound(system)
      case index: String if path.matches("/outbound/(.*)(/?)") => Outbound(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def post(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/inbound(/?)") => Inbound(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def put(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/inbound(/?)") => Inbound(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def patch(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def delete(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def options(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }

  def any(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") => new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") => new NotFoundCall(system)
      case _ => new NotFoundCall(system)
    }
  }
}

class NotFoundCall(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest, msg: HttpContent): Unit = {
    MyHttpResponse.sendNotFound(ctx, "{\"status\": 200, \"message\": \"NOT FOUND: " + path + " \"}")
  }
}


