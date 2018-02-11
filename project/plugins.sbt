import sbt.Resolver

resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("releases")
resolvers += Resolver.bintrayRepo("jroper", "maven")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.6")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "2.0")

addSbtPlugin("com.lightbend.sbt" % "sbt-proguard" % "0.3.0")