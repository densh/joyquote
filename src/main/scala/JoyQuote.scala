package object joy {
  implicit class JoyQuote(ctx: StringContext) {
    def j(args: Joy*): Joy = Joy.parse(ctx.parts.head).get
  }
}
