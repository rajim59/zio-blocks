package zio.blocks.schema.migration

import zio.blocks.schema.{DynamicOptic, DynamicValue, PrimitiveValue}
import zio.blocks.schema.DynamicValue.{Record, Sequence, Variant}
import zio.blocks.schema.migration.MigrationAction._
// We import ConversionOp from our LOCAL SchemaExpr
import zio.blocks.schema.migration.SchemaExpr.ConversionOp 

object MigrationInterpreter {

  def run(data: DynamicValue, action: MigrationAction): Either[MigrationError, DynamicValue] = {
    val nodes = action.at.nodes

    if (nodes.headOption.contains(DynamicOptic.Node.Elements)) {
      data match {
        case Sequence(values) =>
          val subAction = popPath(action)
          val results = values.map(v => run(v, subAction))

          results.find(_.isLeft) match {
            case Some(Left(err)) => Left(prependErrorPath(err, DynamicOptic.Node.Elements))
            case _               => Right(Sequence(results.map(_.toOption.get)))
          }
        case _ => Left(MigrationError.TypeMismatch(action.at, "Sequence", data.getClass.getSimpleName))
      }
    } else {
      nodes.headOption match {
        case Some(fieldNode: DynamicOptic.Node.Field) if shouldRecurse(action) =>
          val fieldName = fieldNode.name
          data match {
            case Record(values) =>
              val index = values.indexWhere(_._1 == fieldName)
              if (index == -1) Left(MigrationError.FieldNotFound(action.at, fieldName))
              else {
                val (key, value) = values(index)
                val subAction    = popPath(action)

                run(value, subAction) match {
                  case Right(newValue) =>
                    Right(Record(values.updated(index, (key, newValue))))
                  case Left(error) =>
                    Left(prependErrorPath(error, fieldNode))
                }
              }
            case _ => Left(MigrationError.TypeMismatch(action.at, "Record", data.getClass.getSimpleName))
          }
        case _ => executeLocal(data, action)
      }
    }
  }

  private def shouldRecurse(action: MigrationAction): Boolean = {
    val nodes = action.at.nodes
    if (nodes.isEmpty) false
    else {
      nodes.head match {
        case _: DynamicOptic.Node.Field =>
          if (nodes.length > 1) true
          else {
            action match {
              case _: Rename | _: AddField | _: DropField => false 
              case _ => true 
            }
          }
        case _ => false
      }
    }
  }

  private def popPath(action: MigrationAction): MigrationAction = {
    def pop(optic: DynamicOptic): DynamicOptic = DynamicOptic(optic.nodes.tail)

    action match {
      case a: Rename            => a.copy(at = pop(a.at))
      case a: AddField          => a.copy(at = pop(a.at))
      case a: DropField         => a.copy(at = pop(a.at))
      case a: TransformValue    => a.copy(at = pop(a.at))
      case a: Mandate           => a.copy(at = pop(a.at))
      case a: Optionalize       => a.copy(at = pop(a.at))
      case a: ChangeType        => a.copy(at = pop(a.at))
      case a: RenameCase        => a.copy(at = pop(a.at))
      case a: TransformElements => a.copy(at = pop(a.at))
      case a: TransformKeys     => a.copy(at = pop(a.at))
      case a: TransformValues   => a.copy(at = pop(a.at))
      case a: Join              => a.copy(at = pop(a.at))
      case a: Split             => a.copy(at = pop(a.at))
      case a: TransformCase     => a.copy(at = pop(a.at))
    }
  }

  private def prependErrorPath(error: MigrationError, node: DynamicOptic.Node): MigrationError = {
    def prepend(optic: DynamicOptic): DynamicOptic = DynamicOptic(node +: optic.nodes)

    error match {
      case e: MigrationError.FieldNotFound => e.copy(path = prepend(e.path))
      case e: MigrationError.TypeMismatch  => e.copy(path = prepend(e.path))
      case other                           => other
    }
  }

  private def executeLocal(data: DynamicValue, action: MigrationAction): Either[MigrationError, DynamicValue] =
    action match {
      case r: Rename =>
        r.at.nodes.lastOption match {
          case Some(DynamicOptic.Node.Field(oldName)) =>
            data match {
              case Record(fields) =>
                val index = fields.indexWhere(_._1 == oldName)
                if (index == -1) Left(MigrationError.FieldNotFound(r.at, oldName))
                else Right(Record(fields.map { case (k, v) => if (k == oldName) (r.to, v) else (k, v) }))
              case _ => Left(MigrationError.TypeMismatch(r.at, "Record", data.getClass.getSimpleName))
            }
          case _ => Left(MigrationError.FieldNotFound(r.at, "unknown"))
        }

      case a: AddField =>
        a.at.nodes.lastOption match {
          case Some(DynamicOptic.Node.Field(fieldName)) =>
            val defaultValue = evaluateExpr(a.default, data)
            data match {
              case Record(fields) => Right(Record(fields :+ (fieldName -> defaultValue)))
              case _              => Left(MigrationError.TypeMismatch(a.at, "Record", data.getClass.getSimpleName))
            }
          case _ => Left(MigrationError.FieldNotFound(a.at, "unknown"))
        }

      case d: DropField =>
        d.at.nodes.lastOption match {
          case Some(DynamicOptic.Node.Field(fieldName)) =>
            data match {
              case Record(fields) =>
                val newFields = fields.filterNot(_._1 == fieldName)
                if (newFields.size == fields.size) Left(MigrationError.FieldNotFound(d.at, fieldName))
                else Right(Record(newFields))
              case _ => Left(MigrationError.TypeMismatch(d.at, "Record", data.getClass.getSimpleName))
            }
          case _ => Left(MigrationError.FieldNotFound(d.at, "unknown"))
        }

      case m: Mandate =>
        val defaultValue = evaluateExpr(m.default, data)
        data match {
          case DynamicValue.Primitive(PrimitiveValue.Unit) => Right(defaultValue)
          case other                                       => Right(other)
        }

      case _: Optionalize => Right(data)

      case rc: RenameCase =>
        data match {
          case Variant(caseName, v) if caseName == rc.from => Right(Variant(rc.to, v))
          case other                                       => Right(other)
        }

      case tv: TransformValue =>
        Right(evaluateExpr(tv.transform, data))

      case ct: ChangeType =>
        Right(evaluateExpr(ct.converter, data))

      case _ => Right(data)
    }

  // Uses local SchemaExpr
  private def evaluateExpr(expr: SchemaExpr[_], data: DynamicValue): DynamicValue = expr match {
    case SchemaExpr.Constant(v)      => v
    case SchemaExpr.DefaultValue(v)  => v
    case SchemaExpr.Identity()       => data 
    case SchemaExpr.Converted(operand, op) => 
      val sourceValue = evaluateExpr(operand, data)
      applyConversion(sourceValue, op)
  }

  private def applyConversion(value: DynamicValue, op: ConversionOp): DynamicValue = {
    (value, op) match {
      case (DynamicValue.Primitive(p), ConversionOp.ToString) => 
        DynamicValue.Primitive(PrimitiveValue.String(p.toString))
      
      case (_, ConversionOp.ToInt) => 
        DynamicValue.Primitive(PrimitiveValue.Int(0)) 

      case _ => value 
    }
  }
}