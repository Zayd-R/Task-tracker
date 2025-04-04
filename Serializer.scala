package models

trait Serializer [A] {
    def serializeToString(value: A): String
    def serializeBack(value: String): Map[Int,A]
    def display(value: Map[Int,A]): String
}
