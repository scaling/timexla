name := "timexla"

version := "0.0.3"

// override def compileOptions = super.compileOptions ++ Seq(Unchecked)

resolvers += Classpaths.typesafeResolver

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0-RC1")
