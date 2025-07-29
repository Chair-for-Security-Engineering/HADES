package hades.namingplugin
import hades.namingplugin.components.MainTransformer

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{ Plugin, PluginComponent }

class NamingPlugin(val global: Global) extends Plugin {
  override val name: String = "hades-naming-plugin"
  override val description: String = "Name signals defined within Template functions"
  override val components: List[PluginComponent] = List(new MainTransformer(global))
}