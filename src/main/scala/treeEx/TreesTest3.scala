class TreesTest3 {
    abstract class Weather
    case class Sunny() extends Weather
    case class Wind(speed : Int) extends Weather
    case class Storm(speed : Int, isTropical: Boolean) extends Weather
    case class Meteorites(area : Int) extends Weather

    abstract class Channel
    case class CNN(event: Event) extends Channel
    case class FoxNews(event: Event) extends Channel

    abstract class Event
    case class WeatherReport(weather : Weather) extends Event
    case class Game() extends Event
    
    abstract class Program
    case class Branch(l : Program, r : Program) extends Program
    case class Leaf(a : Channel) extends Program

    def h(x : Program) : Program = 
    x match {
        case Branch(l, r) => Branch (h(l), h(r))
        case Leaf(a) =>
            val c: Channel = a match{
                case CNN(event) => 
                    val e:Event = event match {
                        case Game() => WeatherReport(Sunny())
                        case WeatherReport(weather) => 
                            val w : Weather = weather match{
                                case Sunny() => Wind(-99)
                                case x => x
                            }
                            WeatherReport(w)
                        }
                    CNN(e)
                case x => x
                }
            Leaf(c)
    }
}
