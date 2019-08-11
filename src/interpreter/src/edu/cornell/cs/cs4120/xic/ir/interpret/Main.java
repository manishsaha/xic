package edu.cornell.cs.cs4120.xic.ir.interpret;

import java.io.PrintWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.Scanner;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import edu.cornell.cs.cs4120.util.CodeWriterSExpPrinter;
import edu.cornell.cs.cs4120.util.SExpPrinter;
import edu.cornell.cs.cs4120.xic.ir.IRBinOp;
import edu.cornell.cs.cs4120.xic.ir.IRBinOp.OpType;
import edu.cornell.cs.cs4120.xic.ir.IRCall;
import edu.cornell.cs.cs4120.xic.ir.IRCompUnit;
import edu.cornell.cs.cs4120.xic.ir.IRConst;
import edu.cornell.cs.cs4120.xic.ir.IRFuncDecl;
import edu.cornell.cs.cs4120.xic.ir.IRMove;
import edu.cornell.cs.cs4120.xic.ir.IRName;
import edu.cornell.cs.cs4120.xic.ir.IRNodeFactory_c;
import edu.cornell.cs.cs4120.xic.ir.IRReturn;
import edu.cornell.cs.cs4120.xic.ir.IRSeq;
import edu.cornell.cs.cs4120.xic.ir.IRStmt;
import edu.cornell.cs.cs4120.xic.ir.IRTemp;
import edu.cornell.cs.cs4120.xic.ir.IRExp;
import edu.cornell.cs.cs4120.xic.ir.parse.IRLexer;
import edu.cornell.cs.cs4120.xic.ir.parse.IRParser;
import edu.cornell.cs.cs4120.xic.ir.visit.CheckCanonicalIRVisitor;
import edu.cornell.cs.cs4120.xic.ir.visit.CheckConstFoldedIRVisitor;

public class Main {
  static String readFile(String path, Charset encoding) throws IOException {
    byte[] encoded = Files.readAllBytes(Paths.get(path));
    return new String(encoded, encoding);
  }

  public static void main(String[] args) {
    try {
      // Scanner sc = new Scanner(System.in);
      // String input = "";
      // while (sc.hasNext()) {
      //   String line = sc.nextLine();
      //   input += sc.nextLine();
      // }
      String input = readFile(args[0], StandardCharsets.UTF_8);

      //System.out.println(input);

      try (StringReader r = new StringReader(input)) {
        IRParser parser = new IRParser(new IRLexer(r), new IRNodeFactory_c());
        IRCompUnit compUnit_ = null;
        try {
          compUnit_ = parser.parse().<IRCompUnit>value();
        } catch (RuntimeException e) {
          throw e;
        } catch (Exception e) {
          // Used by CUP to indicate an unrecoverable error.
          String msg = e.getMessage();
          if (msg != null)
            System.err.println("Syntax error: " + msg);
        }

        if (compUnit_ != null) {
          IRSimulator sim = new IRSimulator(compUnit_);
          sim.call("_Imain_paai", 0);
        }
      }
    } catch (Exception e) {
      System.out.println(e);
    }

  }
}
