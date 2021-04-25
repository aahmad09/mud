package mud

case class Item(name: String, desc: String, damage: Int, delay: Int) {
  val itemName: String = name
  val itemDesc: String = desc
  val itemDamage: Int = damage
  val itemDelay: Int = delay
}