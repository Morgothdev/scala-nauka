package ksiazka

object ala {

    class ScalonaStrona(strony: List[Any]){
    	override def toString = strony.foldLeft("")(_+_+",")
    }
    
    class ZbitkaStron(x: List[ScalonaStrona]) {
        val strony = pierwszaStrona(x)
        def pierwszaStrona(zbitka: List[ScalonaStrona]): List[ScalonaStrona] =
            zbitka.length match {
                case 0 => List()
                case _ => zbitka.last :: zbitka.head :: drugaStrona(zbitka.drop(1).dropRight(1))
            }

        def drugaStrona(zbitka: List[ScalonaStrona]): List[ScalonaStrona] =
            zbitka.length match {
                case 0 => List()
                case _ => zbitka.head :: zbitka.last :: pierwszaStrona(zbitka.drop(1).dropRight(1))
            }
            
        override def toString() = strony.foldLeft("")(_+_)
    }
    
    class Ksiazka(zbitki: List[ZbitkaStron]){
    	override def toString() = zbitki.foldLeft("")(_+_)
    };import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(1204); 

    def scalStrony(strony: List[Any], dzielnik: Int): List[ScalonaStrona] = {
        strony.length match {
            case 0 => List()
            case _ => new ScalonaStrona(strony.slice(0, dzielnik)) :: scalStrony(strony.drop(dzielnik), dzielnik)
        }
    };System.out.println("""scalStrony: (strony: List[Any], dzielnik: Int)List[ksiazka.ala.ScalonaStrona]""");$skip(360); 

    def zbitki(strony: List[ScalonaStrona], dzielnik: Int): List[ZbitkaStron] = {
        if ((dzielnik % 2) != 0) throw new IllegalArgumentException("nie parzyste!"+dzielnik)
        strony.length match {
            case 0 => List()
            case _ => new ZbitkaStron(strony.slice(0, dzielnik)) :: zbitki(strony.drop(dzielnik), dzielnik)
        }
    };System.out.println("""zbitki: (strony: List[ksiazka.ala.ScalonaStrona], dzielnik: Int)List[ksiazka.ala.ZbitkaStron]""");$skip(308); 

    def podziel(stron: Int, kartekWZbitce: Int = 5, stronNaStrone: Int = 2) = {
        val scaloneStrony = scalStrony(1 to stron toList, stronNaStrone)
        val stronWZbitce = kartekWZbitce * 2 * 2
        val zbitkiDoDruku = zbitki(scaloneStrony, stronWZbitce)
        new Ksiazka(zbitkiDoDruku)
    };System.out.println("""podziel: (stron: Int, kartekWZbitce: Int, stronNaStrone: Int)ksiazka.ala.Ksiazka""");$skip(22); val res$0 = 
    podziel(16, 2, 1);System.out.println("""res0: ksiazka.ala.Ksiazka = """ + $show(res$0))}
}
