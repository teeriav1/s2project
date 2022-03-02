package s2project
import o1.{Buffer, Pos}
import s2project.project_gui.visible_objects

import scala.collection.mutable



object constants {
    var player =    new side("Player"   , Pos(400,700))
    var AI =        new side("AI"       , Pos(400,100))
    var sides: Buffer[side] = Buffer(player, AI)
    def debug: Boolean = ???
    def window_width = 800
    def window_height= 800
    def sidebar_width = 400
    def window_title = "Strategy"



    var game_tick = 0
    var game_speed = 1
    def friendly_unit_start_pos = Pos(400,600)
    val enemy_aggression_tick = 999

    var mouseLastPos: Pos = Pos(0,0)
    var compareUnit = new Combat_Unit {
                        override var pos = Pos(0,0)
                        override val owner = AI
                    }

    def base_game_start {
        visible_objects = visible_objects :+ new Base(player.basepos, player)
        visible_objects = visible_objects :+ new Base(AI.basepos, AI)

    }
    def test_game_start = {

        visible_objects = visible_objects :+ new Marine(Pos(150,600), player)
        visible_objects = visible_objects :+ new Marine(Pos(400,600), player)
        visible_objects = visible_objects :+ new Marine(Pos(550,600), player)
        visible_objects = visible_objects :+ new Sniper(Pos(700,600), player)

        visible_objects = visible_objects :+ new Marine(Pos(150,175), AI)
        visible_objects = visible_objects :+ new Marine(Pos(400,175), AI)
        visible_objects = visible_objects :+ new Marine(Pos(550,175), AI)
        visible_objects = visible_objects :+ new Marine(Pos(700,175), AI)
    }
    def game_advance = {
        game_tick += game_speed
        if(player.resource % 10 == 0) {println( player.resource ) }
        if (game_tick % 50 == 0) { player.build("Refinery", None) }
        if (game_tick % 2 == 0) {
        visible_objects.zipWithIndex.foreach{case (x,i) => {
            x match {
                case combat_Unit: Combat_Unit => {
                    if ( combat_Unit.attacking_target_pos.getOrElse(None) != None) {combat_Unit.attack( combat_Unit.attacking_target_pos.get)}
                    else if ( combat_Unit.moving_target_pos.getOrElse(None) != None ) { combat_Unit.move( combat_Unit.moving_target_pos.get)}
                    else { combat_Unit.attack(combat_Unit.pos)}
                }
                case structure: Structure => { structure.tick}
                case unknown => {}
            }}}
        sides.foreach(_.tick )

    }}
    var mouse_pos_list: Buffer[Pos] = o1.Buffer()
    var selected: mutable.Buffer[game_object] = mutable.Buffer()
    def select() = {
        if (mouse_pos_list.length == 0) {} else {
        selected.foreach(_.selected = false)
        selected = Buffer()
        def minX: Double = mouse_pos_list.minBy(_.x).x
        def maxX: Double = mouse_pos_list.maxBy(_.x).x
        def minY: Double = mouse_pos_list.minBy(_.y).y
        def maxY: Double = mouse_pos_list.maxBy(_.y).y
        var clicks_on_field = minX > 0 && minY > 0 && maxX < 800 && maxY < 800

        if (clicks_on_field) {
            selected = visible_objects.filter( one =>one.owner == player && one.pos.x > minX && one.pos.x < maxX && one.pos.y > minY && one.pos.y < maxY )
        }
        mouse_pos_list = Buffer()

        selected.foreach(_.selected = true )
    }}

}
