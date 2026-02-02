package zio.blocks.schema.migration

import zio.blocks.schema.Schema

case class V0(name: String, age: Int)
case class V1(fullName: String, age: Int)

object TestSplit {
  // ১. ডামি স্কিমা (টাইপ সঠিকভাবে ডিফাইন করা)
  implicit val s0: Schema[V0] = null.asInstanceOf[Schema[V0]]
  implicit val s1: Schema[V1] = null.asInstanceOf[Schema[V1]]

  // ২. টাইপ প্যারামিটার স্পষ্টভাবে পাস করা [V0, V1]
  val builder = MigrationBuilder.make[V0, V1]
  
  // ৩. রিনেম ফিল্ড অপারেশন (টাইপ সেফ)
  val step1 = builder.renameField(_.name, _.fullName)

  /** * ৪. বিল্ড মেথড কল করা। 
   * এখানে অটোমেটিক্যালি 'MigrationVerifier[V0, V1, Ops]' জেনারেট হবে।
   */
  val result: Migration[V0, V1] = step1.build
}