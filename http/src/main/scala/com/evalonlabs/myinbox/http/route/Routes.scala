package com.evalonlabs.myinbox.http.route


import io.netty.channel.ChannelHandlerContext
import com.evalonlabs.myinbox.net.http.{ContextSystem, HttpResponse, RouteHandler}
import java.util

object Routes {
  def get(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def post(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def put(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def patch(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def delete(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def options(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }

  def any(system: ContextSystem, path: String): RouteHandler = {
    path match {
      case index: String if path.matches("/") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case page: String if path.matches("/(.*)(/?)") =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
      case _ =>
        new RouteHandler {
          override def handle(ctx: ChannelHandlerContext, path: String, params: util.Map): Unit = {
            HttpResponse.sendNotFound(ctx, "NOT FOUND: " + path)
          }
        }
    }
  }
}



