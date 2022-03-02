package s2project


import o1.gui.{Dialog, Key}
import o1.{Pos, View, White}
import o1.gui.Pic.rectangle
import o1.gui.colors.{Black, Brown, DarkGoldenrod, DarkMagenta, SandyBrown}
import o1.gui.event.KeyReleased
import s2project.constants.{AI, game_tick, mouse_pos_list, player, selected}

import scala.collection.mutable

object project_gui extends App{
    val background = rectangle( constants.window_width,constants.window_height, SandyBrown).leftOf( rectangle( constants.sidebar_width, constants.window_height, White ))
    var visible_objects: mutable.Buffer[game_object] = mutable.Buffer()
    val gui = new View(background,10,constants.window_title) {
        def isOver = false
        def makePic = {
            var background_to_use = background
            visible_objects.zipWithIndex.foreach{ case(x,i) =>{
                background_to_use = background_to_use.place(x.makePic, x.pos)
                                                                }
                                                }
            if(mouse_pos_list.length > 2) { // THIS CREATES EMPTY SQUARE TO SHOW WHAT PLAYER IS CHOOSING ON THE SCREEN
                background_to_use = background_to_use.place( rectangle( 5, mouse_pos_list.maxBy(_.y).y- mouse_pos_list.minBy(_.y).y, o1.Blue ), Pos(mouse_pos_list.minBy(_.x).x, 0.5 * (mouse_pos_list.maxBy(_.y).y+ mouse_pos_list.minBy(_.y).y ) ))
                background_to_use = background_to_use.place( rectangle( 5, mouse_pos_list.maxBy(_.y).y- mouse_pos_list.minBy(_.y).y, o1.Blue ), Pos(mouse_pos_list.maxBy(_.x).x, 0.5 * (mouse_pos_list.maxBy(_.y).y+ mouse_pos_list.minBy(_.y).y ) ))
                background_to_use = background_to_use.place( rectangle( mouse_pos_list.maxBy(_.x).x- mouse_pos_list.minBy(_.x).x,5, o1.Blue ), Pos( 0.5 * (mouse_pos_list.maxBy(_.x).x+ mouse_pos_list.minBy(_.x).x ), mouse_pos_list.minBy(_.y).y ))
                background_to_use = background_to_use.place( rectangle( mouse_pos_list.maxBy(_.x).x- mouse_pos_list.minBy(_.x).x,5, o1.Blue ), Pos( 0.5 * (mouse_pos_list.maxBy(_.x).x+ mouse_pos_list.minBy(_.x).x ), mouse_pos_list.maxBy(_.y).y ))
            }
            background_to_use
            }

        override def isDone= {
         var based = visible_objects.filter( _.name == "Base")
         var owners = based.map(_.owner)
         !(owners.contains(player) && owners.contains(AI))
        }
        override def onTick() = constants.game_advance
        override def onMouseDrag(position: Pos): Unit =  mouse_pos_list = mouse_pos_list :+ position
        override def onMouseUp(position: Pos): Unit = constants.select()
        override def onMouseMove(position: Pos): Unit = constants.mouseLastPos = position
        override def onKeyDown(key: Key): Unit = {
            if (key == Key.A) { constants.selected.zipWithIndex.foreach{case (x,i) => {
                x match {case combat_Unit: Combat_Unit => {
                        combat_Unit.attacking_target_pos =  Some(constants.mouseLastPos)
                        combat_Unit.moving_target_pos = None}
                        case other => {}}}}}
            if (key == Key.S) { constants.selected.zipWithIndex.foreach{case (x,i) => {
                x match {case combat_Unit: Combat_Unit => {
                        combat_Unit.attacking_target_pos =  Some(combat_Unit.pos)
                        combat_Unit.moving_target_pos = None}
                        case other => {}}}}}
            if (key == Key.D) { constants.selected.zipWithIndex.foreach{case (x,i) => {
                x match {case combat_Unit: Combat_Unit => {
                        combat_Unit.attacking_target_pos =  None
                        combat_Unit.moving_target_pos = Some(constants.mouseLastPos)}case other => {}}}}}
            if (key == Key.Q) {
                if(player.resource > 50) {
                    player.resource -= 50
                    player.build( "Base", Some(constants.mouseLastPos) )
                }
            }

            } }
    constants.base_game_start
    constants.test_game_start
    gui.start()
}
