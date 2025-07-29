package hades.namingplugin.components

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform

class MainTransformer(val global: Global) extends PluginComponent with Transform {

  override val phaseName: String = "hades-naming-plugin"

  override val runsAfter: List[String] = List("idsl-plugin")
  override val runsRightAfter: Option[String] = Some("idsl-plugin")

  override protected def newTransformer(unit: global.CompilationUnit): global.Transformer = HadesNamingTransformer

  import global._

  object HadesNamingTransformer extends Transformer {

    def symbolHasAnnotation(s: Symbol, name: String): Boolean = {
      if (s.annotations.exists(_.symbol.name.toString() == name)) return true
      s.parentSymbols.exists(symbolHasAnnotation(_, name))
    }

    def symbolHasTrait(s: Symbol, name: String): Boolean = {
      s.parentSymbols.exists { p =>
        (p.fullName == name) || symbolHasTrait(p, name)
      }
    }

    def typeHasTrait(s: Type, name: String): Boolean = {
      s.parents.exists { p =>
        p.toString().toString == name  || typeHasTrait(p, name)
      }
    }

    private def mclass(sym: Symbol) = sym map (_.asModule.moduleClass)

    private def transformInstantiate(tree: global.Tree, clazz: Symbol, func: Symbol): global.Tree = {
      tree match {

        // Recurse into blocks
        case b: Block =>
          treeCopy.Block(b, b.stats.map(transformInstantiate(_, clazz, func)), transformInstantiate(b.expr, clazz, func))

        // Recurse into if-statements
        case i: If =>
          treeCopy.If(i, i.cond, transformInstantiate(i.thenp, clazz, func), transformInstantiate(i.elsep, clazz, func))

        // Name variables
        case vd: ValDef =>

          val nameStr = vd.getterName.toString()
          val const = Constant(nameStr)
          val lit = Literal(const)
          val thiz = This(clazz)
          val sel = Select(thiz, func)
          val appl = Apply(sel, List(vd.rhs, lit))

          thiz.tpe = clazz.tpe
          sel.tpe = func.tpe
          appl.tpe = definitions.UnitTpe
          lit.setType(definitions.StringTpe)

          atOwner(tree.symbol) {
            treeCopy.ValDef(vd, vd.mods, vd.name, vd.tpt, appl)
          }

        case _ => tree
      }
    }

    override def transform(tree: global.Tree): global.Tree = {
      tree match {
        // Match HADES template classes and transform instantiate functions
        case cd: ClassDef if symbolHasTrait(cd.symbol, "spinal.core.internals.Template") =>

          val clazz = cd.impl.symbol.owner
          val func = clazz.tpe.members.find(_.name.toString == "valCallback").get
          val body = cd.impl.body.map {
            case dd: DefDef if dd.name.string_==("instantiate") || symbolHasAnnotation(dd.symbol, "Name") =>
              atOwner(dd.symbol) {
                treeCopy.DefDef(dd, dd.mods, dd.name, dd.tparams, dd.vparamss, dd.tpt, transformInstantiate(dd.rhs, clazz, func))
              }

            case t => t
          }

          val impl = treeCopy.Template(cd.impl, cd.impl.parents, cd.impl.self, body)
          atOwner(cd.symbol) {
            treeCopy.ClassDef(cd, cd.mods, cd.name, cd.tparams, impl)
          }

        // Recurse through packages
        case PackageDef(pid, stats) =>
          treeCopy.PackageDef(
            tree,
            transform(pid).asInstanceOf[RefTree],
            atOwner(mclass(tree.symbol)) {
              transformStats(stats, currentOwner)
            }
          )

        case _ => tree
      }
    }
  }
}