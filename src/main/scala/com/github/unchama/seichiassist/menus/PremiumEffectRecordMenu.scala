package com.github.unchama.seichiassist.menus

import cats.effect.IO
import com.github.unchama.buildassist.MenuInventoryData
import com.github.unchama.concurrent.BukkitSyncIOShift
import com.github.unchama.itemstackbuilder.SkullItemStackBuilder
import com.github.unchama.menuinventory.slot.button.Button
import com.github.unchama.menuinventory.slot.button.action.{ClickEventFilter, FilteredButtonEffect}
import com.github.unchama.menuinventory.{Menu, MenuFrame, MenuSlotLayout}
import com.github.unchama.seichiassist.SeichiAssist
import com.github.unchama.targetedeffect.{computedEffect, sequentialEffect}
import com.github.unchama.targetedeffect.player.PlayerEffects.openInventoryEffect
import com.github.unchama.targetedeffect.player.{FocusedSoundEffect, PlayerEffects}
import org.bukkit.{ChatColor, Sound}
import org.bukkit.entity.Player

class PremiumEffectRecordMenu extends Menu {
  import com.github.unchama.menuinventory.syntax._
  /**
   * メニューのサイズとタイトルに関する情報
   */
  override val frame = MenuFrame(4.chestRows, ChatColor.BLUE + "" + ChatColor.BOLD + "プレミアムエフェクト購入履歴")

  /**
   * @return `player`からメニューの[[MenuSlotLayout]]を計算する[[IO]]
   */
  override def computeMenuLayout(player: Player): IO[MenuSlotLayout] = IO {
    val legacyInventory = SeichiAssist.databaseGateway.donateDataManipulator.calculateDonateInventory(SeichiAssist.playermap(player.getUniqueId))
    val theItem = new SkullItemStackBuilder("MHF_ArrowLeft")
      .lore(List(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"))
      .build()
    legacyInventory.setItem(27, theItem)
    import scala.jdk.CollectionConverters._
    val base = legacyInventory
      .iterator()
      .asScala
      .zipWithIndex
      .map(k => (k._2, Button(k._1 /* TODO: Check if NOP */)))
      .toMap
    MenuSlotLayout(base)
  }
}
