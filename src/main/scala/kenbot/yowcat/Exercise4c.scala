package kenbot.yowcat


/**
 * Exercise 4c. 
 *
 * It's turtles all the way down!
 *
 * In the category of (small[1]) categories, functors themselves are the arrows!
 *
 * Let's build it. To keep it simple, lets just use Cat1 and Cat2, from Exercise 4b. 
 */
object CatCat extends Cat {
  type Obj = Cat 
  override def objects: Stream[Cat] = Stream(Cat1, Cat2)

  type Arr = Functor
  def arrows: Stream[Functor] = for {
    from <- objects
    to <- objects
  } yield functor(from, to)

  private def functor(from: Cat, to: Cat) =
    new Functor {
      val dom = from
      val cod = to

      def mapArr(arr: dom.Arr): cod.Arr = {
        cod.compose(cod.id(mapObj(dom.dom(arr))), cod.id(mapObj(dom.cod(arr))))
      }

      def mapObj(obj: dom.Obj): cod.Obj = {
        val i = dom.objects.indexOf(obj)
        cod.objects.drop(i).head
      }
    }

  def dom(f: Functor): Cat = f.dom
  def cod(f: Functor): Cat = f.cod

  /** 
   * Because this representation of a category sacrifices type safety so that we can 
   * manipulate objects as values, you will need to perform a type cast here 
   * to make the arrows click together.
   *
   * This is obviously unnecessary in the usual type-safe usage.
   * 
   * Syntax: foo.asInstanceOf[Bar] 
   */
  override def comp(f: Functor, g: Functor): Functor =
    new Functor {
      val dom = f.dom
      val cod = g.cod

      def mapArr(arr: dom.Arr): cod.Arr = {
        val a: dom.Obj = dom.dom(arr)
        val b: dom.Obj = dom.cod(arr)
        val c: cod.Obj = a.asInstanceOf[cod.Obj]
        val d: cod.Obj = b.asInstanceOf[cod.Obj]
        cod.compose(cod.id(c), cod.id(d))
      }

      def mapObj(obj: dom.Obj): cod.Obj = {
        val r1: f.cod.Obj = f.mapObj(obj.asInstanceOf[f.dom.Obj])
        val r2: g.cod.Obj = g.mapObj(r1.asInstanceOf[g.dom.Obj])
        r2.asInstanceOf[cod.Obj]
      }
    }

  def id(cat: Cat): Functor = functor(cat, cat)
}

/* [1]
 *
 * A "small category" means that the objects form a Set, and the arrows form a Set.
 *
 * In particular, the category of Sets is a "large category" 
 * (because there is no such thing as a Set of Sets). Our "category of categories" here is
 * also a large category -- and so thankfully, doesn't include itself.
 * 
 * Such is the price of dodging paradoxes! We needn't let this foundational ducking and weaving 
 * bother us for these exercises though.
 */
