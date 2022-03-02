package s2project
import scala.util.Random.nextInt
import o1.{Pic, Pos}
import o1.gui.Color
import o1.gui.Pic.{circle, rectangle}
import o1.gui.colors.Black

import scala.collection.mutable
import scala.math.sqrt
import scala.util.Random

class visual_effect(x: Int, y: Int, image: Pic) {
    // THERE ARE TIMES WHEN VISUAL EFFECTS ARE CALCULATED OUTSIDE WHERE THEY ARE DRAWN
}

class side( val name: String, var basepos: Pos) {
    // SIDES WILL ALSO BE REFERRED WITH NAMES PLAYER AND AI WHICH ARE THE TWO SIDES
    override def toString: String = name
    var resource = 0
    var resource_rate = 1

    def build(name: String, possible_place: Option[Pos]) = {
        // THIS GIVE SEMI RANDOM PLACE FOR A STRUCTURE TO BE BUILT AND ADDS THE STRUCTURE TO GAME
        var builder = this
        def own_structures = project_gui.visible_objects.filter( x => x.owner == builder && x.speed == 0 )

        var building: Option[Structure] = None
        def build_pos(owner: side,  possible_place: Option[Pos]): o1.Pos = {
            if( possible_place.getOrElse(None) == None ){
            var closest_structure = own_structures(nextInt( own_structures.length) )
            def randomise_placement = -50 + nextInt(100)
            Pos(closest_structure.pos.x + randomise_placement, closest_structure.pos.y + randomise_placement)
            } else { possible_place.get } }
        if (name == "Refinery")  { building = Some( new Refinery( build_pos(builder, possible_place), builder))}
        if (name == "Base")  { building = Some( new Base( build_pos(builder, possible_place), builder))}
        if (building.getOrElse(None) == None) {} else {
        project_gui.visible_objects = project_gui.visible_objects :+ building.get
    }}
    def tick = {
        this.resource += resource_rate
    }
}
trait game_object {
    // GAME OBJECT CAN BE UNIT/LIVE ONE OR STUCTURE/STATIC ONE
    def width: Int
    def color: Color
    def speed: Int = 0
    def makePic: Pic
    var pos: Pos
    val owner: side
    val can_move = false
    val name: String
    var health: Double
    def die = {}
    var selected: Boolean = false
    def take_damage(amount: Int) = this.health -= amount
    override def toString: String = owner+"'s "+name
}
trait Combat_Unit extends game_object {
    var attacking_target_pos: Option[Pos] = None
    var moving_target_pos: Option[Pos] = None
    override var health: Double = 100.0
    var damage = 10
    var last_target_x_diff: Double = 0
    var last_target_y_diff: Double = 1
    def moving_rotation(xdiff: Double, ydiff: Double): Double = {
        // at least one is 0
        if (xdiff == 0 && ydiff == 0) {0}
        else if(xdiff == 0) { if(ydiff > 0) {0} else {180}}
        else if(ydiff == 0) { if (xdiff > 0) {90} else {270}}

        //neither is 0 // 4 squares of 2 dimensions
        else if (xdiff > 0 && ydiff <0) { 90 * (xdiff)/(xdiff - ydiff)}
        else if (xdiff > 0 && ydiff > 0){ 90+90* (ydiff / (ydiff + xdiff)) }
        else if (ydiff  > 0 && xdiff < 0) { 180 + 90* (xdiff * -1)/(ydiff - xdiff)}
        else if (xdiff <0 && ydiff < 0) { 270 + 90 * (ydiff)/(ydiff + xdiff)}
        else {0}
    }
    override def speed = 10

    def original_pic: Pic = { if(this.selected) {
        rectangle(width/2,5, Black).clockwise(90).above(circle( width,  color).onto(circle( width+10,  o1.Blue) ) ) } else {
        rectangle(width/2,5, Black).clockwise(90).above(circle( width,  color) ) } }

    override def makePic: Pic = original_pic.clockwise( this.moving_rotation( last_target_x_diff, last_target_y_diff ) )
    override def color: Color = if (owner.name == "AI") { o1.Red} else { o1.Green}
    override def width: Int = 30
    override val name = "Combat Unit"
    override val can_move: Boolean = true
    val range = 100
    val collision_range = 35
    def is_enemy_on_range: Boolean = {
        // SEEKS UNIT LIST AND CHECKS IF THERE IS ANY TARGETS
        var answer = false
        project_gui.visible_objects.zipWithIndex.foreach{ case (x,i) => {
            if (this.pos.distance( x.pos ) < this.range && this.owner != x.owner) { answer = true}
        }}
        answer
    }
    def get_target: Option[game_object] = {
        // TAKES ANY TARGET THAT IS POSSIBLE TO TAKE
        project_gui.visible_objects.find( x => x.pos.distance( this.pos ) < range && x.owner != this.owner )
    }
    def attack(target: Pos) = {
        // SHOULD ONE ATTACK
        if (!is_enemy_on_range) {
            this.move( target )
        } else {
        // ACTUAL ATTACK
            var enemy = this.get_target.get
            last_target_x_diff = ( enemy.pos.x - this.pos.x ).toInt
            last_target_y_diff = ( enemy.pos.y - this.pos.y ).toInt
            enemy.take_damage( this.damage )
            println( enemy + " took "+damage.toString+" damage to "+enemy.health+" health!" )
            if (enemy.health <= 0) {
                enemy.die
                project_gui.visible_objects = project_gui.visible_objects.filter(_ != enemy )
            }
        }
    }
    def check_collision(target: Pos, direction: String): Boolean = {
        // step 1: Is anyone too close
        var return_value = false
        val units_near: mutable.Buffer[game_object] = project_gui.visible_objects.filter(x=> x.pos.distance( this.pos ) < collision_range && x != this)
        if (units_near.isEmpty) { false } else { // if nobody is near, no need to check directions
        // Is this moving closer or further
        // direction if either x or y
                units_near.zipWithIndex.foreach { case (near_one, i) => {
                    if (direction == "y") {
                        val target_higher = target.y > this.pos.y
                        val collisionhigher = near_one.pos.y > this.pos.y
                        if (target_higher == collisionhigher ) { return_value = true}
                    } else {
                        val target_higher = target.x > this.pos.x
                        val collisionhigher = near_one.pos.x > this.pos.x
                        if (target_higher == collisionhigher ) { return_value = true}
                    }
                }
            }
        }
        return_value
    }
    def move(target: Pos) = {
        if (target == this.pos) {}
        else if (this.pos.distance(target) < speed ) {this.pos = target }
        else {
        // Move splits amount of distance to be travelled between x and y
        def  distance_x = { // distance should always be at least 0
            if(target.x > this.pos.x) {target.x - this.pos.x}
            else { this.pos.x - target.x}
        }
        def  distance_y = { // distance should always be at least 0
            if(target.y > this.pos.y) {target.y - this.pos.y}
            else { this.pos.y - target.y}
        }
        last_target_x_diff = target.x - this.pos.x
        last_target_y_diff = target.y - this.pos.y
        // splitting speed between x and y
        var max_distance_to_travel_x = (this.speed * ( distance_x / (distance_x + distance_y)))
        var max_distance_to_travel_y = (this.speed * ( distance_y / (distance_x + distance_y)))
        // Goal is to move towards target. If target is close, no need to go past

        val this_bigger_x = this.pos.x > target.x
        val this_bigger_y = this.pos.y > target.y
        // INTRODUCE COLLISION
        // ACTUAL MOVING
        if( !check_collision(target,"x") ) {
        if (this_bigger_x) { this.pos = this.pos.addX(-max_distance_to_travel_x) }
        else { this.pos = this.pos.addX( max_distance_to_travel_x) }
        }
        if( !check_collision(target,"y") ) {
        if (this_bigger_y) { this.pos = this.pos.addY(-max_distance_to_travel_y)}
        else {this.pos =  this.pos.addY( max_distance_to_travel_y )}
        }
    }
}}
class Marine( startpos: Pos, side: side) extends Combat_Unit {
    // MARINE IS LIKE THE BASIC UNIT TO CONTROL IN THE GAME
    override var pos = startpos
    override val owner = side
    override val name = "Marine"
}
class Sniper( startpos: Pos, side: side) extends Combat_Unit {
    override var pos = startpos
    override val owner = side
    override val name = "Sniper"

    override def original_pic: Pic = { if(this.selected) {
        rectangle(width,5, Black).clockwise(90).above(circle( width,  color).onto(circle( width+10,  o1.Blue) ) ) } else {
        rectangle(width,5, Black).clockwise(90).above(circle( width,  color) ) } }
    override val range: Int = 200
    override val speed = (super.speed * 0.6).toInt
}
trait Structure extends game_object {
    override var health: Double = 400
    var active = false
    override def makePic: Pic = rectangle( width, height, color)
    override def color: Color = if (owner.name == "AI") { o1.Purple } else { o1.Blue}
    override def width: Int = 150
    def height: Int = 50
    def move(target: Pos) = {}
    def tick = {} // TICK IS THE FUNCTION OF STRUCTURE
}
class Base( startpos: Pos, side: side) extends Structure {
    override var pos = startpos
    override val owner = side
    override val name = "Base"
}
class Refinery( startpos: Pos, side: side) extends Structure {
    override def makePic = {
        if(constants.game_tick % 2 ==0) {super.makePic.onto( rectangle( width+5, height+5, o1.Green) ) } else {super.makePic}
    }
    override var pos = startpos
    override val owner = side
    override val name = "Refinery"
    override def width: Int = 50
    override def tick = owner.resource += owner.resource_rate
}