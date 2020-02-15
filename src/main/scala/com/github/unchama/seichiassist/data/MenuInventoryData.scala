package com.github.unchama.seichiassist.data

import java.util
import java.util.{Collections, UUID}

import com.github.unchama.seichiassist.{LevelThresholds, SeichiAssist}
import com.github.unchama.seichiassist.achievement.Nicknames
import com.github.unchama.seichiassist.activeskill.effect.{ActiveSkillNormalEffect, ActiveSkillPremiumEffect}
import com.github.unchama.seichiassist.data.player.PlayerData
import com.github.unchama.seichiassist.task.VotingFairyTask
import com.github.unchama.seichiassist.util.{AsyncInventorySetter, ItemMetaFactory, TypeConverter}
import org.bukkit.{Bukkit, ChatColor, Material}
import org.bukkit.enchantments.Enchantment
import org.bukkit.entity.Player
import org.bukkit.inventory.{Inventory, ItemFlag, ItemStack}
import org.bukkit.inventory.meta.{ItemMeta, SkullMeta}

import scala.jdk.CollectionConverters._
import scala.collection.mutable


object MenuInventoryData {
  private val playermap = SeichiAssist.playermap
  private val databaseGateway = SeichiAssist.databaseGateway
  // 実際には60人も入ることは無いのでは？
  private val finishedHeadPageBuild = new mutable.HashMap[UUID, Boolean](60, 0.75)
  private val finishedMiddlePageBuild = new mutable.HashMap[UUID, Boolean](60, 0.75)
  private val finishedTailPageBuild = new mutable.HashMap[UUID, Boolean](60, 0.75)
  private val finishedShopPageBuild = new mutable.HashMap[UUID, Boolean](60, 0.75)
  private val headPartIndex = new mutable.HashMap[UUID, Int](60, 0.75)
  private val middlePartIndex = new mutable.HashMap[UUID, Int](60, 0.75)
  private val tailPartIndex = new mutable.HashMap[UUID, Int](60, 0.75)
  private val shopIndex = new mutable.HashMap[UUID, Int](60, 0.75)
  private val taihiIndex = new mutable.HashMap[UUID, Int](60, 0.75)
  private val loreTable = util.Arrays.asList(
    Collections.emptyList,
    util.Arrays.asList(ChatColor.RED + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ガンガンたべるぞ", ChatColor.RESET + "" + ChatColor.GRAY + "とにかく妖精さんにりんごを開放します。", ChatColor.RESET + "" + ChatColor.GRAY + "めっちゃ喜ばれます。"),
    util.Arrays.asList(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "バッチリたべよう", ChatColor.RESET + "" + ChatColor.GRAY + "食べ過ぎないように注意しつつ", ChatColor.RESET + "" + ChatColor.GRAY + "妖精さんにりんごを開放します。", ChatColor.RESET + "" + ChatColor.GRAY + "喜ばれます。"),
    util.Arrays.asList(ChatColor.GREEN + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "リンゴだいじに", ChatColor.RESET + "" + ChatColor.GRAY + "少しだけ妖精さんにりんごを開放します。", ChatColor.RESET + "" + ChatColor.GRAY + "伝えると大抵落ち込みます。"),
    util.Arrays.asList(ChatColor.BLUE + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "リンゴつかうな", ChatColor.RESET + "" + ChatColor.GRAY + "絶対にりんごを開放しません。", ChatColor.RESET + "" + ChatColor.GRAY + "")
  ).asScala
  /**
   * (short) 3はダサいし、マジックコンスタントみたいだよね。
   */
  private val PLAYER_SKULL: Short = 3
  /**
   * ラムダをいちいち正確に打つのは退屈で疲れる作業だし、かといってメソッドでカプセル化するメリットもない。
   * 加えて、明示的に「まとめる」ことでJVMに対して最適化のヒントとしても使える。
   */
  private val DIG100 = (meta: ItemMeta) => {meta.addEnchant(Enchantment.DIG_SPEED, 100, false); Unit}
  private val toMoveNicknameMenu = build(Material.BARRIER, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "二つ名組合せメインメニューへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動")
  private val ARROW_LEFT = "MHF_ArrowLeft"
  private val ARROW_UP = "MHF_ArrowUp"
  private val GB_CONFIRM = util.Arrays.asList(
    ChatColor.RESET + "" + ChatColor.GREEN + "進化することにより、スキルの秘めたる力を解放できますが",
    ChatColor.RESET + "" + ChatColor.GREEN + "スキルは更に大量の魂を求めるようになり",
    ChatColor.RESET + "" + ChatColor.GREEN + "レベル(回復確率)がリセットされます",
    ChatColor.RESET + "" + ChatColor.RED + "本当に進化させますか?",
    ChatColor.RESET + "" + ChatColor.DARK_RED + ChatColor.UNDERLINE + "クリックで進化させる"
  )
  //投票特典受け取りボタン
  private def getVoteButtonLore(playerdata: PlayerData) = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GRAY + "投票特典を受け取るには", ChatColor.RESET + "" + ChatColor.GRAY + "投票ページで投票した後", ChatColor.RESET + "" + ChatColor.GRAY + "このボタンをクリックします", ChatColor.RESET + "" + ChatColor.AQUA + "特典受取済投票回数：" + playerdata.p_givenvote, ChatColor.RESET + "" + ChatColor.AQUA + "所有投票pt：" + playerdata.activeskilldata.effectpoint)

  /**
   * 整地量
   *
   * @param page ページ
   * @return メニュー
   */
  def getRankingBySeichiAmount(page: Int) = {
    val pageLimit = 14
    val lowerBound = 100
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "整地神ランキング")
    val itemstack = new ItemStack(Material.SKULL_ITEM, 1, PLAYER_SKULL)
    var invIndex = 0
    for (rank <- 10 * page until 10 + 10 * page) {
      if (rank >= SeichiAssist.ranklist.size) break //todo: break is not supported
      val rankdata = SeichiAssist.ranklist(rank)
      if (rankdata.totalbreaknum < LevelThresholds.levelExpThresholds(lowerBound - 1)) { //レベル100相当の総整地量判定に変更
        break //todo: break is not supported
      }
      val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "整地レベル:" + rankdata.level, ChatColor.RESET + "" + ChatColor.GREEN + "総整地量:" + rankdata.totalbreaknum)
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.BOLD + "" + (rank + 1) + "位:" + "" + ChatColor.WHITE + rankdata.name, lore, rankdata.name)
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, invIndex, itemstack.clone)
      invIndex += 1
    }
    if (page != pageLimit) { // 整地神ランキング次ページ目を開く
      val skullMeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "整地神ランキング" + (page + 2) + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), "MHF_ArrowDown")
      itemstack.setItemMeta(skullMeta)
      AsyncInventorySetter.setItemAsync(inventory, 52, itemstack.clone)
    }

    // 1ページ目を開く
    val firstPage = page == 0
    val name = if (firstPage) ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ホームへ" else ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "整地神ランキング" + page + "ページ目へ";
    {
      val lore = Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動")
      val ign = if (firstPage) ARROW_LEFT else ARROW_UP
      val skullmeta = buildSkullMeta(name, lore, ign)
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, 45, itemstack.clone)
    }

    // 総整地量の表記
    {
      val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.AQUA + "全プレイヤー総整地量:", ChatColor.RESET + "" + ChatColor.AQUA + SeichiAssist.allplayerbreakblockint)
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "整地鯖統計データ", lore, "unchama")
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, 53, itemstack.clone)
    }
    inventory
  }

  /**
   * ログイン時間
   *
   * @param page ページ
   * @return メニュー
   */
  def getRankingByPlayingTime(page: Int) = {
    val pageLimit = 14
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "ログイン神ランキング")
    val itemstack = new ItemStack(Material.SKULL_ITEM, 1, PLAYER_SKULL)
    val rankStart = 10 * page
    var invIndex = 0
    for (rank <- rankStart until rankStart + 10) {
      if (rank >= SeichiAssist.ranklist_playtick.size) break //todo: break is not supported
      val rankdata = SeichiAssist.ranklist_playtick(rank)
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.BOLD + "" + (rank + 1) + "位:" + "" + ChatColor.WHITE + rankdata.name, Collections.singletonList(ChatColor.RESET + "" + ChatColor.GREEN + "総ログイン時間:" + TypeConverter.toTimeString(TypeConverter.toSecond(rankdata.playtick))), rankdata.name)
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, invIndex, itemstack.clone)
      invIndex += 1
    }
    if (page != pageLimit) {
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ログイン神ランキング" + (page + 2) + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), "MHF_ArrowDown")
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, 52, itemstack.clone)
    }
    // 前のページ / ホームへ
    var skullmeta = null
    if (page == 0) skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ホームへ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), ARROW_LEFT)
    else { // 整地神ランキング前ページを開く;
      skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ログイン神ランキング" + page + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), ARROW_UP)
    }
    itemstack.setItemMeta(skullmeta)
    AsyncInventorySetter.setItemAsync(inventory, 45, itemstack.clone)
    inventory
  }

  /**
   * 投票回数
   *
   * @param page ページ
   * @return メニュー
   */
  def getRankingByVotingCount(page: Int) = {
    val pageLimit = 14
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "投票神ランキング")
    val itemstack = new ItemStack(Material.SKULL_ITEM, 1, PLAYER_SKULL)
    var voteRank = 10 * page
    var invIndex = 0
    while (voteRank < 10 + 10 * page) {
      if (voteRank >= SeichiAssist.ranklist_p_vote.size) break //todo: break is not supported
      val rankdata = SeichiAssist.ranklist_p_vote(voteRank)
      if (rankdata.p_vote == 0) break //todo: break is not supported
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.BOLD + "" + (voteRank + 1) + "位:" + "" + ChatColor.WHITE + rankdata.name, Collections.singletonList(ChatColor.RESET + "" + ChatColor.GREEN + "総投票回数:" + rankdata.p_vote), rankdata.name)
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, invIndex, itemstack.clone)
      voteRank += 1
      invIndex += 1
    }
    if (page != pageLimit) { // 投票神ランキング次ページ目を開く
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "投票神ランキング" + (page + 2) + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), "MHF_ArrowDown")
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, 52, itemstack.clone)
    }
    var skullmeta = null
    if (page == 0) skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ホームへ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), ARROW_LEFT)
    else { // 投票神ランキング前ページを開く
      skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "投票神ランキング" + page + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), ARROW_UP)
    }
    itemstack.setItemMeta(skullmeta)
    AsyncInventorySetter.setItemAsync(inventory, 45, itemstack.clone)
    inventory
  }

  /**
   * プレミアムエフェクトポイント
   *
   * @param page ページ
   * @return メニュー
   */
  def getRankingByPremiumEffectPoint(page: Int) = {
    val pageLimit = 2
    val lowerBound = 1
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "寄付神ランキング")
    val itemstack = new ItemStack(Material.SKULL_ITEM, 1, PLAYER_SKULL)
    var donationRank = 50 * page
    var invIndex = 0
    while ( {
      donationRank < 50 + 50 * page
    }) {
      if (donationRank >= SeichiAssist.ranklist_premiumeffectpoint.size) break //todo: break is not supported
      val rankdata = SeichiAssist.ranklist_premiumeffectpoint(donationRank)
      if (rankdata.premiumeffectpoint < lowerBound) { //寄付金額0
        break //todo: break is not supported
      }
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.BOLD + "" + (donationRank + 1) + "位:" + "" + ChatColor.WHITE + rankdata.name, Collections.singletonList(ChatColor.RESET + "" + ChatColor.GREEN + "総寄付金額:" + rankdata.premiumeffectpoint * 100), rankdata.name)
      itemstack.setItemMeta(skullmeta)
      var finalInventoryIndex = 0
      if (invIndex == 45) finalInventoryIndex = 47
      else finalInventoryIndex = invIndex
      AsyncInventorySetter.setItemAsync(inventory, finalInventoryIndex, itemstack.clone)
      donationRank += 1
      invIndex += 1
    }
    if (page != pageLimit) {
      val skullmeta = buildSkullMeta(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "寄付神ランキング" + (page + 2) + "ページ目へ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), "MHF_ArrowDown")
      itemstack.setItemMeta(skullmeta)
      AsyncInventorySetter.setItemAsync(inventory, 52, itemstack.clone)
    }
    val skullmeta = if (page == 0) {
      buildSkullMeta(
        ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ホームへ",
        Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"),
        ARROW_LEFT
      )
    } else { // 寄付神ランキング前ページ目を開く;
      buildSkullMeta(
        ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "寄付神ランキング" + page + "ページ目へ",
        Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"),
        ARROW_UP
      )
    }
    itemstack.setItemMeta(skullmeta)
    AsyncInventorySetter.setItemAsync(inventory, 45, itemstack.clone)
    inventory
  }

  /**
   * エフェクト選択
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def getActiveSkillEffectMenuData(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    //念のためエラー分岐
    if (validate(p, playerdata, "整地スキルエフェクト選択")) return null
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "整地スキルエフェクト選択");
    val itemstack1: ItemStack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "スキルメニューへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", ARROW_LEFT)
    AsyncInventorySetter.setItemAsync(inventory, 45, itemstack1)
    //1行目
    val lore = List(ChatColor.RESET + "" + ChatColor.GREEN + "現在選択しているエフェクト：" + ActiveSkillNormalEffect.getNameByNum(playerdata.activeskilldata.effectnum), ChatColor.RESET + "" + ChatColor.YELLOW + "使えるエフェクトポイント：" + playerdata.activeskilldata.effectpoint, ChatColor.RESET + "" + ChatColor.DARK_GRAY + "※投票すると獲得出来ます", ChatColor.RESET + "" + ChatColor.LIGHT_PURPLE + "使えるプレミアムポイント：" + playerdata.activeskilldata.premiumeffectpoint, ChatColor.RESET + "" + ChatColor.DARK_GRAY + "※寄付をすると獲得できます")
    val itemstack2 = buildPlayerSkull(
      ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + playerdata.lowercaseName + "のスキルエフェクトデータ",
      lore.asJava,
      // この操作は安全; メニューを開けているのにUUIDがないなんてことがないから
      Bukkit.getOfflinePlayer(playerdata.uuid).getName,
      x => DIG100(x)
    )
    AsyncInventorySetter.setItemAsync(inventory, 0, itemstack2.clone)
    val itemstack3 = build(Material.BOOK_AND_QUILL, ChatColor.BLUE + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "プレミアムエフェクト購入履歴", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで閲覧")
    AsyncInventorySetter.setItemAsync(inventory, 2, itemstack3)
    val itemstack4 = build(Material.GLASS, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "エフェクトを使用しない", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックでセット", x => DIG100(x))
    AsyncInventorySetter.setItemAsync(inventory, 1, itemstack4);
    {
      var i = 0
      ActiveSkillNormalEffect.values.foreach { elem =>
        //プレイヤーがそのスキルを取得している場合の処理
        val itemstack = if (playerdata.activeskilldata.obtainedSkillEffects.contains(elem))
          build(elem.material, elem.nameOnUI, util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + elem.explanation, ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックでセット"))
        else { //プレイヤーがそのスキルをまだ取得していない場合の処理
          build(Material.BEDROCK, null, util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + elem.explanation, ChatColor.RESET + "" + ChatColor.YELLOW + "必要エフェクトポイント：" + elem.usePoint, ChatColor.RESET + "" + ChatColor.AQUA + "" + ChatColor.UNDERLINE + "クリックで解除"))
        }
        AsyncInventorySetter.setItemAsync(inventory, i, itemstack)
        i += 1
      }
    }

    {
      val effects = ActiveSkillPremiumEffect.values
      var i = 27
      for (effect <- effects) {
        val itemstack = if (playerdata.activeskilldata.obtainedSkillPremiumEffects.contains(effect)) {
          build(effect.material, ChatColor.UNDERLINE + "" + ChatColor.BOLD + ChatColor.stripColor(effect.desc), util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + effect.explain, ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックでセット"))
        } else {
          build(Material.BEDROCK, effect.desc, util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + effect.explain, ChatColor.RESET + "" + ChatColor.YELLOW + "必要プレミアムポイント：" + effect.usePoint, ChatColor.RESET + "" + ChatColor.AQUA + "" + ChatColor.UNDERLINE + "クリックで解除"))
        }
        AsyncInventorySetter.setItemAsync(inventory, i, itemstack)
        i += 1
      }
    }
    inventory
  }

  /**
   * プレミア購入履歴表示
   *
   * @param player プレイヤー
   * @return メニュー
   */
  def getBuyRecordMenuData(player: Player) = {
    // TODO: PremiumEffectRecordMenu.scalaへの統合
    val inventory = getEmptyInventory(4, ChatColor.BLUE + "" + ChatColor.BOLD + "プレミアムエフェクト購入履歴")
    val itemstack = buildPlayerSkull(null, ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", ARROW_LEFT)
    AsyncInventorySetter.setItemAsync(inventory, 27, itemstack.clone)
    val playerdata = playermap(player.getUniqueId)
    databaseGateway.donateDataManipulator.loadDonateData(playerdata, inventory)
    inventory
  }

  /**
   * 二つ名組み合わせ
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def setFreeTitleMainData(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "二つ名組み合わせ")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "二つ名組合せシステム")
    //各ボタンの設定
    finishedHeadPageBuild.put(uuid, false)
    finishedMiddlePageBuild.put(uuid, false)
    finishedTailPageBuild.put(uuid, false)
    finishedShopPageBuild.put(uuid, false)
    headPartIndex.put(uuid, 0)
    middlePartIndex.put(uuid, 0)
    tailPartIndex.put(uuid, 0)
    shopIndex.put(uuid, 0)
    taihiIndex.put(uuid, 0)
    //実績ポイントの最新情報反映ボタン
    // dynamic button
    val lore = List(ChatColor.RESET + "" + ChatColor.GREEN + "クリックで情報を最新化", ChatColor.RESET + "" + ChatColor.RED + "累計獲得量：" + playerdata.achievePoint.cumulativeTotal, ChatColor.RESET + "" + ChatColor.RED + "累計消費量：" + playerdata.achievePoint.used, ChatColor.RESET + "" + ChatColor.AQUA + "使用可能量：" + playerdata.achievePoint.left)
    val itemstack = build(Material.EMERALD_ORE, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "実績ポイント 情報", lore.asJava)
    AsyncInventorySetter.setItemAsync(inventory, 0, itemstack)
    //パーツショップ
    // const button
    val itemstack2 = build(Material.ITEM_FRAME, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "実績ポイントショップ", ChatColor.RESET + "" + ChatColor.GREEN + "クリックで開きます")
    AsyncInventorySetter.setItemAsync(inventory, 9, itemstack2)
    //エフェクトポイントからの変換ボタン
    val lore2 = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.RED + "JMS投票で手に入るポイントを", ChatColor.RESET + "" + ChatColor.RED + "実績ポイントに変換できます。", ChatColor.RESET + "" + ChatColor.YELLOW + "" + ChatColor.BOLD + "投票pt 10pt → 実績pt 3pt", ChatColor.RESET + "" + ChatColor.AQUA + "クリックで変換を一回行います。", ChatColor.RESET + "" + ChatColor.GREEN + "所有投票pt :" + playerdata.activeskilldata.effectpoint, ChatColor.RESET + "" + ChatColor.GREEN + "所有実績pt :" + playerdata.achievePoint.left)
    val itemstack3 = build(Material.EMERALD, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ポイント変換ボタン", lore2)
    AsyncInventorySetter.setItemAsync(inventory, 1, itemstack3)
    val nickname = playerdata.settings.nickname
    val playerTitle = Nicknames.getTitleFor(nickname.id1, nickname.id2, nickname.id3)
    val itemStack = build(Material.BOOK, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "現在の二つ名の確認", ChatColor.RESET + "" + ChatColor.RED + "「" + playerTitle + "」")
    AsyncInventorySetter.setItemAsync(inventory, 4, itemStack)
    val toHeadSelection = build(Material.WATER_BUCKET, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "前パーツ選択画面", ChatColor.RESET + "" + ChatColor.RED + "クリックで移動します")
    val toMiddleSelection = build(Material.MILK_BUCKET, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "中パーツ選択画面", ChatColor.RESET + "" + ChatColor.RED + "クリックで移動します")
    val toTailSelection = build(Material.LAVA_BUCKET, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "後パーツ選択画面", ChatColor.RESET + "" + ChatColor.RED + "クリックで移動します")
    AsyncInventorySetter.setItemAsync(inventory, 11, toHeadSelection)
    AsyncInventorySetter.setItemAsync(inventory, 13, toMiddleSelection)
    AsyncInventorySetter.setItemAsync(inventory, 15, toTailSelection)
    // const Button
    val itemstack4 = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "実績・二つ名メニューへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", ARROW_LEFT)
    AsyncInventorySetter.setItemAsync(inventory, 27, itemstack4.clone)
    inventory
  }

  /**
   * 二つ名 - 前パーツ
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def setFreeTitle1Data(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "二つ名/前パーツ")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "二つ名組合せ「前」")
    if (finishedHeadPageBuild.getOrElse(uuid, false)) finishedHeadPageBuild.put(uuid, false)
    else headPartIndex.put(uuid, 1000)
    //解禁済みの実績をチェック→前パーツがあるかをチェック→あればボタン配置
    var inventoryIndex = 0
    for (i <- headPartIndex(uuid) until 9900) {
      if (inventoryIndex < 27) if (playerdata.TitleFlags.contains(i)) {
        val maybeHeadPart = Nicknames.getHeadPartFor(i)
        if (maybeHeadPart.nonEmpty) {
          val itemstack = build(Material.WATER_BUCKET, Integer.toString(i), ChatColor.RESET + "" + ChatColor.RED + "前パーツ「" + maybeHeadPart.get + "」")
          AsyncInventorySetter.setItemAsync(inventory, inventoryIndex, itemstack)
          inventoryIndex += 1
        }
      }
      else if (inventoryIndex == 27) { //次ページへのボタンを配置
        val itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "次ページへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", "MHF_ArrowRight")
        AsyncInventorySetter.setItemAsync(inventory, 27, itemstack)
        finishedHeadPageBuild.put(uuid, true)
        break //todo: break is not supported
      }
    }
    //パーツ未選択状態にするボタン
    // Pure Button
    val itemstack = build(Material.GRASS, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "前パーツを未選択状態にする", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで実行")
    AsyncInventorySetter.setItemAsync(inventory, 31, itemstack)

    // 二つ名組合せメインページボタン
    AsyncInventorySetter.setItemAsync(inventory, 27, toMoveNicknameMenu)
    inventory
  }

  private def validate(destination: Player, pd: PlayerData, operation: String): Boolean = {
    if (pd == null) {
      destination.sendMessage(ChatColor.RED + "playerdataがありません。管理者に報告してください")
      Bukkit.getServer.getConsoleSender.sendMessage(ChatColor.RED + "SeichiAssist[" + operation + "]でエラー発生")
      Bukkit.getLogger.warning(destination + "のplayerdataがありません。開発者に報告してください")
      return true
    }
    false
  }

  /**
   * 二つ名 - 中パーツ
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def setFreeTitle2Data(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "二つ名/中パーツ")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "二つ名組合せ「中」")
    if (finishedMiddlePageBuild.getOrElse(uuid, false)) finishedMiddlePageBuild.put(uuid, false)
    else middlePartIndex.put(uuid, 9900)
    //パーツがあるかをチェック→あればボタン配置
    var inventoryIndex = 0
    for (i <- middlePartIndex(uuid) until 9999) {
      if (inventoryIndex < 27) {
        val maybeMiddlePart = Nicknames.getMiddlePartFor(i)
        //一部の「隠し中パーツ」は取得しているかの確認
        if (maybeMiddlePart.nonEmpty || 9911 <= i && playerdata.TitleFlags.contains(i)) {
          val itemstack = build(Material.MILK_BUCKET, Integer.toString(i), ChatColor.RESET + "" + ChatColor.RED + "中パーツ「" + maybeMiddlePart.get + "」")
          AsyncInventorySetter.setItemAsync(inventory, inventoryIndex, itemstack)
          inventoryIndex += 1
        }
      }
      else if (inventoryIndex == 27) {
        val lore = Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動")
        val itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "次ページへ", lore, "MHF_ArrowRight")
        AsyncInventorySetter.setItemAsync(inventory, 35, itemstack.clone)
        finishedMiddlePageBuild.put(uuid, true)
        break //todo: break is not supported
      }
    }
    val itemstack = build(Material.GRASS, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "中パーツを未選択状態にする", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで実行")
    AsyncInventorySetter.setItemAsync(inventory, 31, itemstack)

    // 二つ名組合せメインページを開くボタン
    AsyncInventorySetter.setItemAsync(inventory, 27, toMoveNicknameMenu)
    inventory
  }

  /**
   * 二つ名 - 後パーツ
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def setFreeTitle3Data(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "二つ名/後パーツ")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "二つ名組合せ「後」")
    if (!finishedTailPageBuild.getOrElse(uuid, false)) tailPartIndex.put(uuid, 1000)
    //解禁済みの実績をチェック→後パーツがあるかをチェック→あればボタン配置
    var inventoryIndex = 0
    for (i <- tailPartIndex(uuid) until 9900) {
      if (inventoryIndex < 27) if (playerdata.TitleFlags.contains(i)) {
        val maybeTailPart = Nicknames.getTailPartFor(i)
        if (maybeTailPart.nonEmpty) {
          val itemstack = build(Material.LAVA_BUCKET, Integer.toString(i), ChatColor.RESET + "" + ChatColor.RED + "後パーツ「" + maybeTailPart.get + "」")
          AsyncInventorySetter.setItemAsync(inventory, inventoryIndex, itemstack)
          inventoryIndex += 1
        }
      }
      else if (inventoryIndex == 27) {
        val itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "次ページへ", Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動"), "MHF_ArrowRight")
        AsyncInventorySetter.setItemAsync(inventory, 35, itemstack.clone)
        finishedTailPageBuild.put(uuid, true)
        break //todo: break is not supported
      }
    }
    val itemstack = build(Material.GRASS, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "後パーツを未選択状態にする", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで実行")
    AsyncInventorySetter.setItemAsync(inventory, 31, itemstack)
    AsyncInventorySetter.setItemAsync(inventory, 27, toMoveNicknameMenu)
    inventory
  }

  /**
   * 実績ポイントショップ
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def setTitleShopData(p: Player): Inventory = { //プレイヤーを取得
    val uuid = p.getUniqueId
    //プレイヤーデータ
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "実績ポイントショップ")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "実績ポイントショップ")
    val ap = playerdata.achievePoint
    val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "クリックで情報を最新化", ChatColor.RESET + "" + ChatColor.RED + "累計獲得量：" + ap.cumulativeTotal, ChatColor.RESET + "" + ChatColor.RED + "累計消費量：" + ap.used, ChatColor.RESET + "" + ChatColor.AQUA + "使用可能量：" + ap.left)
    val itemstack: ItemStack = build(Material.EMERALD_ORE, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "実績ポイント 情報", lore)
    AsyncInventorySetter.setItemAsync(inventory, 0, itemstack)

    //おしながき
    if (playerdata.samepageflag) shopIndex.put(uuid, taihiIndex(uuid))
    else if (!finishedShopPageBuild.getOrElse(uuid, false)) shopIndex.put(uuid, 9801)
    taihiIndex.put(uuid, shopIndex(uuid))
    playerdata.samepageflag_$eq(false)
    var inventoryIndex = 1
    var forNextI = 0
    for (i <- shopIndex(uuid) to 9833) {
      var itemstack: ItemStack = null
      if (inventoryIndex < 27) if (!playerdata.TitleFlags.contains(i)) {
        val lore: mutable.Buffer[String] = mutable.Buffer(
          ChatColor.RESET + "" + ChatColor.RED + "前・後パーツ「" + Nicknames.getHeadPartFor(i).getOrElse(() => "") + "」",
          ChatColor.RESET + "" + ChatColor.GREEN + "必要ポイント：20",
          ChatColor.RESET + "" + ChatColor.AQUA + "クリックで購入できます"
        )
        itemstack = build(Material.BEDROCK, Integer.toString(i), lore.asJava)
        AsyncInventorySetter.setItemAsync(inventory, inventoryIndex, itemstack)
        inventoryIndex += 1
      }
      else {
        itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "次ページへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", "MHF_ArrowRight")
        AsyncInventorySetter.setItemAsync(inventory, 35, itemstack.clone)
        finishedShopPageBuild.put(uuid, true)
        forNextI = i
        break //todo: break is not supported
      }
    }
    // SAFE: putしているのでキーがないなんてことはない
    shopIndex.put(uuid, Math.max(forNextI, 9911))
    for (i <- shopIndex(uuid) to 9938) {
      if (inventoryIndex < 27) {
        if (!playerdata.TitleFlags.contains(i)) {
          val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.RED + "中パーツ「" + Nicknames.getMiddlePartFor(i).getOrElse(() => "") + "」", ChatColor.RESET + "" + ChatColor.GREEN + "必要ポイント：35", ChatColor.RESET + "" + ChatColor.AQUA + "クリックで購入できます")
          val itemstack = build(Material.BEDROCK, Integer.toString(i), lore)
          AsyncInventorySetter.setItemAsync(inventory, inventoryIndex, itemstack)
          inventoryIndex += 1
        }
        else { //次ページへ遷移するボタン
          val itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "次ページへ", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動", "MHF_ArrowRight")
          AsyncInventorySetter.setItemAsync(inventory, 35, itemstack)
          finishedShopPageBuild.put(uuid, true)
          break //todo: break is not supported
        }
      }
    }
    AsyncInventorySetter.setItemAsync(inventory, 27, toMoveNicknameMenu)
    inventory
  }

  /**
   * 投票妖精メニュー
   *
   * @param p プレイヤー
   * @return メニュー
   */
  def getVotingMenuData(p: Player): Inventory = { //UUID取得
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "投票妖精")) return null
    val inventory = getEmptyInventory(4, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "投票ptメニュー");
    //投票pt受け取り
    {
      val itemstack = build(
        Material.DIAMOND,
        ChatColor.LIGHT_PURPLE + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "クリックで投票特典を受け取れます",
        getVoteButtonLore(playerdata),
        x => DIG100(x)
        )
      AsyncInventorySetter.setItemAsync(inventory, 0, itemstack)
    }

    // ver0.3.2 投票ページ表示
    {
      val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "投票すると様々な特典が！", ChatColor.RESET + "" + ChatColor.GREEN + "1日1回投票出来ます", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "クリックするとチャット欄に", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "URLが表示されますので", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "Tキーを押してから", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "そのURLをクリックしてください")
      val itemstack = build(Material.BOOK_AND_QUILL, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "投票ページにアクセス", lore)
      AsyncInventorySetter.setItemAsync(inventory, 9, itemstack)
    }

    //棒メニューに戻る
    {
      val lore = Collections.singletonList(ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで移動")
      val itemstack = buildPlayerSkull(ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "ホームへ", lore, ARROW_LEFT)
      AsyncInventorySetter.setItemAsync(inventory, 27, itemstack.clone)
    }

    //妖精召喚時間設定トグルボタン
    {
      val list = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "" + ChatColor.BOLD + "" + VotingFairyTask.dispToggleVFTime(playerdata.toggleVotingFairy), "", ChatColor.RESET + "" + ChatColor.GRAY + "コスト", ChatColor.RESET + "" + ChatColor.RED + "" + ChatColor.BOLD + "" + playerdata.toggleVotingFairy * 2 + "投票pt", "", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで切替")
      val itemStack = build(Material.WATCH, ChatColor.AQUA + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "マナ妖精 時間設定", list)
      AsyncInventorySetter.setItemAsync(inventory, 2, itemStack)
    }

    //妖精契約設定トグル
    {
      val itemStack = new ItemStack(Material.PAPER)
      itemStack.setItemMeta(getVotingFairyContractMeta(playerdata))
      AsyncInventorySetter.setItemAsync(inventory, 11, itemStack)
    }
    //妖精音トグル
    {
      val itemStack = new ItemStack(Material.JUKEBOX)
      itemStack.setItemMeta(getVotingFairySoundsToggleMeta(playerdata.toggleVFSound))
      AsyncInventorySetter.setItemAsync(inventory, 20, itemStack)
    }

    //妖精召喚
    {
      val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GRAY + "" + playerdata.toggleVotingFairy * 2 + "投票ptを消費して", ChatColor.RESET + "" + ChatColor.GRAY + "マナ妖精を呼びます", ChatColor.RESET + "" + ChatColor.GRAY + "時間 : " + VotingFairyTask.dispToggleVFTime(playerdata.toggleVotingFairy), ChatColor.RESET + "" + ChatColor.DARK_RED + "Lv.10以上で解放")
      val itemStack = build(Material.GHAST_TEAR, ChatColor.LIGHT_PURPLE + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "マナ妖精 召喚", lore, x => DIG100(x))
      AsyncInventorySetter.setItemAsync(inventory, 4, itemStack)
    }

    {
      if (playerdata.usingVotingFairy) { //妖精 時間確認
      {
        val lore = util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GRAY + "妖精さんはいそがしい。", ChatColor.GRAY + "帰っちゃう時間を教えてくれる")
        val itemStack = build(Material.COMPASS, ChatColor.LIGHT_PURPLE + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "マナ妖精に時間を聞く", lore, x => DIG100(x))
        AsyncInventorySetter.setItemAsync(inventory, 13, itemStack)
      }
      val yourRank = playerdata.calcPlayerApple()
      val lore = new util.ArrayList[String](6 + 2 * 4 + 5)
      // 6
      lore.addAll(util.Arrays.asList(ChatColor.RESET + "" + ChatColor.RED + "" + ChatColor.BOLD + "※ﾆﾝｹﾞﾝに見られないように気を付けること！", ChatColor.RESET + "" + ChatColor.RED + "" + ChatColor.BOLD + "  毎日大妖精からデータを更新すること！", "", ChatColor.RESET + "" + ChatColor.GOLD + "" + ChatColor.BOLD + "昨日までにがちゃりんごを", ChatColor.RESET + "" + ChatColor.GOLD + "" + ChatColor.BOLD + "たくさんくれたﾆﾝｹﾞﾝたち", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "召喚されたらラッキーだよ！"))
      for (rank <- 0 to 3) {
        if (rank >= SeichiAssist.ranklist_p_apple.size) break //todo: break is not supported
        val rankdata = SeichiAssist.ranklist_p_apple(rank)
        if (rankdata.p_apple == 0) break //todo: break is not supported
        // 2 x 4 = 8
        lore.add(ChatColor.GRAY + "たくさんくれたﾆﾝｹﾞﾝ第" + (rank + 1) + "位！")
        lore.add(ChatColor.GRAY + "なまえ：" + rankdata.name + " りんご：" + rankdata.p_apple + "個")
      }
      // 5
      lore.add(ChatColor.AQUA + "ぜーんぶで" + SeichiAssist.allplayergiveapplelong + "個もらえた！")
      lore.add("")
      lore.add(ChatColor.GREEN + "↓呼び出したﾆﾝｹﾞﾝの情報↓")
      lore.add(ChatColor.GREEN + "今までに" + playerdata.p_apple + "個もらった")
      lore.add(ChatColor.GREEN + "ﾆﾝｹﾞﾝの中では" + yourRank + "番目にたくさんくれる！")
      val itemStack = build(Material.GOLDEN_APPLE, ChatColor.YELLOW + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "㊙ がちゃりんご情報 ㊙", lore, x => DIG100(x))
      AsyncInventorySetter.setItemAsync(inventory, 6, itemStack)
      }
    }
    inventory
  }

  /**
   * 投票妖精音切り替え
   *
   * @param playSound trueなら鳴らす
   * @return ラベルがついたアイテム
   */
  private def getVotingFairySoundsToggleMeta(playSound: Boolean) = {
    val itemmeta = Bukkit.getItemFactory.getItemMeta(Material.JUKEBOX)
    itemmeta.setDisplayName(ChatColor.GOLD + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "マナ妖精の音トグル")
    if (!playSound) {
      DIG100(itemmeta)
    }
    val lore = if (playSound) {
      util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "現在音が鳴る設定になっています。", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "※この機能はデフォルトでONです。", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで切替")
    } else {
      util.Arrays.asList(ChatColor.RESET + "" + ChatColor.RED + "現在音が鳴らない設定になっています。", ChatColor.RESET + "" + ChatColor.DARK_GRAY + "※この機能はデフォルトでONです。", ChatColor.RESET + "" + ChatColor.DARK_RED + "" + ChatColor.UNDERLINE + "クリックで切替")
    }
    itemmeta.setLore(lore)
    itemmeta
  }

  /**
   * 投票妖精戦略
   *
   * @param playerdata プレイヤーの設定
   * @return ラベルが付いたアイテム
   */
  private def getVotingFairyContractMeta(playerdata: PlayerData) = {
    val itemmeta = Bukkit.getItemFactory.getItemMeta(Material.PAPER)
    itemmeta.setDisplayName(ChatColor.GOLD + "" + ChatColor.UNDERLINE + "" + ChatColor.BOLD + "妖精とのお約束")
    // n % 4 + 1 -> 1..4
    val strategy = playerdata.toggleGiveApple
    val lore = loreTable(strategy)
    itemmeta.setLore(lore.asInstanceOf[util.List[String]])
    itemmeta
  }

  /**
   * GiganticBerserk進化設定
   *
   * @param p
   * @return メニュー
   */
  def getGiganticBerserkBeforeEvolutionMenu(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "Gigantic進化前確認")) return null
    val inventory = getEmptyInventory(6, ChatColor.DARK_PURPLE + "" + ChatColor.BOLD + "スキルを進化させますか?")
    // 色
    val table = Array[Byte](12, 15, 4, 0, 3);
    {
      val itemstack = new ItemStack(Material.STAINED_GLASS_PANE, 1, table(playerdata.giganticBerserk.stage))
      val itemmeta = itemstack.getItemMeta
      itemmeta.setDisplayName(" ")
      itemstack.setItemMeta(itemmeta)
      // dynamic
      placeGiganticBerserkGlass(inventory, itemstack)
      // static
      placeGiganticBerserkShape(inventory)
    }
    val lore = GB_CONFIRM
    val itemstack = build(Material.NETHER_STAR, ChatColor.WHITE + "スキルを進化させる", lore)
    AsyncInventorySetter.setItemAsync(inventory, 31, itemstack)
    inventory
  }

  def getGiganticBerserkAfterEvolutionMenu(p: Player): Inventory = {
    val uuid = p.getUniqueId
    val playerdata = SeichiAssist.playermap(uuid)
    if (validate(p, playerdata, "GiganticBerserk進化後画面")) return null
    val inventory = getEmptyInventory(6, ChatColor.LIGHT_PURPLE + "" + ChatColor.BOLD + "スキルを進化させました")
    val table = Array[Byte](12, 15, 4, 0, 3, 12)
    val b = table(playerdata.giganticBerserk.stage)
    val itemstack = new ItemStack(Material.STAINED_GLASS_PANE, 1, b)
    val itemmeta = itemstack.getItemMeta
    if (playerdata.giganticBerserk.stage >= 4) {
      itemmeta.addEnchant(Enchantment.DAMAGE_ALL, 1, true)
      itemmeta.addItemFlags(ItemFlag.HIDE_ENCHANTS)
    }
    itemmeta.setDisplayName(" ")
    itemstack.setItemMeta(itemmeta)
    placeGiganticBerserkGlass(inventory, itemstack)
    placeGiganticBerserkShape(inventory)
    val itemstack2 = build(Material.NETHER_STAR, ChatColor.WHITE + "スキルを進化させました！", util.Arrays.asList(ChatColor.RESET + "" + ChatColor.GREEN + "スキルの秘めたる力を解放することで、マナ回復量が増加し", ChatColor.RESET + "" + ChatColor.DARK_RED + "スキルはより魂を求めるようになりました"))
    AsyncInventorySetter.setItemAsync(inventory, 31, itemstack2)
    inventory
  }

  private def getEmptyInventory(rows: Int, title: String): Inventory = Bukkit.getServer.createInventory(null, rows * 9, title)

  private def buildSkullMeta(name: String, lore: util.List[String], owner: String): SkullMeta = {
    val ret = ItemMetaFactory.SKULL.getValue
    ret.setDisplayName(name)
    ret.setOwner(owner)
    ret.setLore(lore)
    ret
  }

  private def build(mat: Material, name: String, singleLore: String): ItemStack  = build(mat, name, singleLore, nullConsumer)

  private def build[T <: ItemMeta](mat: Material, name: String, singleLineLore: String, modify: T => Unit): ItemStack = build(mat, name, Collections.singletonList(singleLineLore), modify)

  private def build(mat: Material, name: String, lore: util.List[String]): ItemStack = build(mat, name, lore, nullConsumer)

  private def build[T <: ItemMeta](mat: Material, name: String, lore: util.List[String], modify: T => Unit): ItemStack  = {
    val temp = new ItemStack(mat)
    // 自己責任。
    @SuppressWarnings(Array("unchecked")) val meta = temp.getItemMeta.asInstanceOf[T]
    if (name != null) meta.setDisplayName(name)
    if (lore != null) meta.setLore(lore)
    meta.addItemFlags(ItemFlag.HIDE_ATTRIBUTES)
    modify(meta)
    temp.setItemMeta(meta)
    temp
  }

  private def buildPlayerSkull(name: String, lore: String, owner: String): ItemStack = buildPlayerSkull(name, Collections.singletonList(lore), owner)

  private def buildPlayerSkull(name: String, lore: util.List[String], owner: String): ItemStack = buildPlayerSkull(name, lore, owner, nullConsumer)

  private def buildPlayerSkull(name: String, lore: util.List[String], owner: String, modify: SkullMeta => Unit) = {
    val ret = new ItemStack(Material.SKULL_ITEM, 1, PLAYER_SKULL)
    val sm = ItemMetaFactory.SKULL.getValue
    if (name != null) sm.setDisplayName(name)
    if (lore != null) sm.setLore(lore)
    sm.setOwner(owner)
    modify(sm)
    ret.setItemMeta(sm)
    ret
  }

  private def nullConsumer[T] = (nothing: T) => {
    def foo(nothing: T) = {
    }

    foo(nothing)
  }

  private def placeGiganticBerserkShape(inv: Inventory) = {
    val itemstack = build(Material.STICK, " ", null.asInstanceOf[String])
    for (i <- Array[Int](30, 39, 40, 47)) {
      AsyncInventorySetter.setItemAsync(inv, i, itemstack)
    }
  }

  private def placeGiganticBerserkGlass(inv: Inventory, itemstack: ItemStack) = for (i <- Array[Int](6, 7, 14, 15, 16, 21, 22, 23, 24, 32, 41)) {
    AsyncInventorySetter.setItemAsync(inv, i, itemstack)
  }
}

final class MenuInventoryData private() {
}
