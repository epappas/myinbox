package com.evalonlabs.myinbox.http.route


import io.netty.channel.ChannelHandlerContext
import com.evalonlabs.myinbox.net.http.{ContextSystem, HttpResponse => MyHttpResponse, RouteHandler}
import java.util
import com.evalonlabs.myinbox.http.route.call.IndexCall
import io.netty.handler.codec.http.HttpRequest

object Routes {
  def get(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        IndexCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def post(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def put(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def patch(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def delete(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def options(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }

  def any(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new NotFoundCall(system)
      case page: String if path.matches("/(.*)(/?)") =>
        new NotFoundCall(system)
      case _ =>
        new NotFoundCall(system)
    }
  }
}

class NotFoundCall(system: ContextSystem) extends RouteHandler {

  override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map[String, Object], request: HttpRequest): Unit = {
    MyHttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
  }
}


