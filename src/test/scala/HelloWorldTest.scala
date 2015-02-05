package example

import org.scalatest._
import org.scalatest.FunSuite
import java.io.File
import org.xml.sax.helpers.DefaultHandler
import org.scalatest.Assertions._
import org.xml.sax.SAXParseException
import scala.util.Try
import scala.collection.mutable.ListBuffer

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import javax.xml.validation.Validator;

 class Validator extends DefaultHandler {
    var validationError = false
    var numErrors = 0
    var exceptions = ListBuffer[SAXParseException]()
    var saxParseException: SAXParseException = null
    override def error(exception: SAXParseException) {
      //println("XML SCHEMA ERROR: " + exception);
      numErrors += 1
     validationError = true;
     saxParseException = exception;
     exceptions += exception
    }     
    override def fatalError(exception: SAXParseException) {
     // println("XML SCHEMA ERROR: " + exception);
     validationError = true;      
     saxParseException = exception; 
     exceptions += exception
    }       
    override def warning(exception: SAXParseException) { }
  }
  

class HelloWorldTest extends FunSuite {

  val executableName = "test_HelloWorldTest.exe"
  
  //"The x86 reference XML file" should "validate with the schema" in {
//   val parser = new SAXParser()
//    parser.setFeature("http://xml.org/sax/features/validation", true); 
//    parser.setFeature("http://apache.org/xml/features/validation/schema", true);
//    parser.setFeature("http://apache.org/xml/features/validation/schema-full-checking", true);
    val xsd = new File("x86.xsd")
    val x86ref = new File("x86reference.xml")
//    parser.setProperty(
//     "http://apache.org/xml/properties/schema/external-noNamespaceSchemaLocation",
//     xsd.toURI.toString);
    
    val sf = SchemaFactory.newInstance(
      "http://www.w3.org/XML/XMLSchema/v1.1");
    val s = sf.newSchema(xsd);
    val v = s.newValidator();
    val src = new StreamSource(x86ref);
    val handler = new Validator();
    v.setErrorHandler(handler)
    
    //val handler = new Validator();
    //parser.setErrorHandler(handler);
    

    val result: Try[List[SAXParseException]] = Try{v.validate(src); handler.exceptions.toList}
    val failures = result getOrElse handler.exceptions.toList
    var i = 0
    failures.take(10) foreach { x =>
      test("XML Schema validation failure " + i) {
        assert(false, "- Validation error on line " + x.getLineNumber + ": " + x.getMessage)
      }
      i+=1
    }
  
}