class Animal
class Dog(name: String) extends Animal

class InvariantList[T]

class ContravariantList[-T]

trait Vet[-T <: Animal] {
  def heal(animal: T): Boolean
}

object Main extends App {
  val lassie = new Dog("Lassie")
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")

  // Dog <: Animal
  val anAnimal: Animal = lassie
  // Dog <: Animal then List[Dog] <: List[Animal] because List[+A] where +A means covariant
  val myDogs: List[Animal] = List(lassie, hachi, laika)

  // InvariantList[T] - T is not covariant then InvariantList[B] where B <: A is not a subtype of InvariantList[A]
  val invariantMyDogs: InvariantList[Animal] = new InvariantList[Dog]

  // ContravariantList[-T] - A <: B becomes B <: A
  val contravariantMyDogs: ContravariantList[Dog] =
    new ContravariantList[Animal]
}
