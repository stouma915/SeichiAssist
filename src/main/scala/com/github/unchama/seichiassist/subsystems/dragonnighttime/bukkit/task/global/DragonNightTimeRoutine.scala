package com.github.unchama.seichiassist.subsystems.dragonnighttime.bukkit.task.global

import java.util.{Calendar, TimeZone}

import cats.effect.{IO, Timer}
import com.github.unchama.concurrent.{RepeatingRoutine, RepeatingTaskContext}
import com.github.unchama.seichiassist.subsystems.dragonnighttime.bukkit.task.DragonNightTimeTask
import com.github.unchama.seichiassist.util.Util

import scala.concurrent.duration.FiniteDuration

object DragonNightTimeRoutine {
  def apply()(implicit context: RepeatingTaskContext): IO[Nothing] = {
    val getRepeatInterval: IO[FiniteDuration] = IO {
      import scala.concurrent.duration._

      1.seconds
    }

    val routineAction = IO {
      val time = LocalTime.of(20, 0, 0)

      if (LocalTime.now().equals(time)) {
        DragonNightTimeTask.startDragonNightTime()

        Util.sendEveryMessage("ドラゲナイタイム開始！")
        Util.sendEveryMessage("採掘速度上昇Lv10のバフが1時間付与されました")
      }
    }

    implicit val timer: Timer[IO] = IO.timer(context)

    RepeatingRoutine.permanentRoutine(getRepeatInterval, routineAction)
  }
}