import com.ibm.icu.text._;
import java.io._;

object Norm{

    def readFile(fileName:String): String = {
        val file = new File(fileName);
        val len = file.length;
        val inp = new FileInputStream(file);
        val buffer = new Array[Byte](len.toInt);
        inp.read(buffer);
        val data = new String(buffer, "UTF-8");
        inp.close;
        return data;
           
    }
    def writeFile(fileName:String, data:String) = {
        val wr = new FileOutputStream(fileName);
        try {            
            wr.write(data.getBytes("UTF-8"));
        }
        finally{
            wr.close();
        }
    }

    def main(args : Array[String]){
        if(args.length < 2){
            println("Usage: norm {nfc|nfd|nfkc|nfkd} <input-files>")
            System.exit(1);
        }
        val normalizer = args(0) match {
            case "nfc" => Normalizer2.getNFCInstance;
            case "nfd" => Normalizer2.getNFDInstance;
            case "nfkc" => Normalizer2.getNFKCInstance;
            case "nfkd" => Normalizer2.getNFKDInstance;
            case _ => throw new Exception("no normalization form: "+args(0))
        }
        for(arg <- args.slice(1, args.length)){
            val input = readFile(arg);
            val result = normalizer.normalize(input);
            writeFile(arg+"_icu_"+args(0), result);
        }
    }
}