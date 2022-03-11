import mill._, scalalib._
import $ivy.`com.lihaoyi::mill-contrib-bloop:0.10.1`

object main extends ScalaModule {
  def scalaVersion = "3.1.1"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::scalatags:0.11.1"
  )
  def scalacOption = Seq("-Yexplicit-nulls", "-language:strict-equality", "-deprecation")
}
