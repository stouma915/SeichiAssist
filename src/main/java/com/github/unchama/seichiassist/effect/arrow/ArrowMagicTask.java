package com.github.unchama.seichiassist.effect.arrow;

import com.github.unchama.seichiassist.SeichiAssist;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.Sound;
import org.bukkit.entity.Player;
import org.bukkit.entity.ThrownPotion;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.PotionMeta;
import org.bukkit.potion.PotionData;
import org.bukkit.potion.PotionType;
import org.bukkit.util.Vector;

public class ArrowMagicTask extends AbstractEffectTask<ThrownPotion> {
	public ArrowMagicTask(Player player) {
		//ポーションデータを生成
		ItemStack i = new ItemStack(Material.SPLASH_POTION);
		PotionMeta pm = (PotionMeta) Bukkit.getItemFactory().getItemMeta(Material.SPLASH_POTION);
		pm.setBasePotionData(new PotionData(PotionType.INSTANT_HEAL));
		i.setItemMeta(pm);

		//プレイヤーの位置を取得
		Location ploc = player.getLocation();
		//発射する音を再生する.
		player.playSound(ploc, Sound.ENTITY_WITCH_THROW, 1, 1.3f);

		//スキルを実行する処理
		Location loc = player.getLocation().clone();
		loc.add(loc.getDirection()).add(getAdditionalVector());
		Vector vec = loc.getDirection().multiply(getVectorMultiplier());
		projectile = player.getWorld().spawn(loc, ThrownPotion.class);
		SeichiAssist.entitylist.add(projectile);
		projectile.setShooter(player);
		projectile.setGravity(false);
		projectile.setItem(i);
		launchProjectile(vec);
	}

	@Override
	public void run() {

	}

	@Override
	public Vector getAdditionalVector() {
		return new Vector(0, 1.6, 0);
	}

	@Override
	public double getVectorMultiplier() {
		return 0.8;
	}


}
