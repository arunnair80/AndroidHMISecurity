package com.github.darkkey.androidnuitst;

import java.net.ServerSocket;
import java.net.Socket;
import java.io.*;
import java.io.IOException;
import java.util.Hashtable;

import java.awt.image.BufferedImage;

import android.graphics.*;

import com.ericsson.otp.erlang.*;

import com.android.uiautomator.core.UiObject;
import com.android.uiautomator.core.UiObjectNotFoundException;
import com.android.uiautomator.core.UiScrollable;
import com.android.uiautomator.core.UiSelector;
import com.android.uiautomator.testrunner.UiAutomatorTestCase;

public class AndroidNUItest extends UiAutomatorTestCase {

    Hashtable<String, byte[]> screensCache = new Hashtable<String, byte[]>();
    int cropHeight = 50;
    String tempPath = "/data/local/tmp";

    byte[] getScreen() throws IOException{
            String tempFilename = tempPath + "/tmpscr.png";
            File screenFile = new File(tempFilename);
            getUiDevice().takeScreenshot(screenFile);

            Bitmap myBitmap = BitmapFactory.decodeFile(tempFilename);
            Bitmap newBitmap = Bitmap.createBitmap(myBitmap, 0, cropHeight, myBitmap.getWidth(), myBitmap.getHeight() - cropHeight);

            FileOutputStream out = new FileOutputStream(tempFilename);
            newBitmap.compress(Bitmap.CompressFormat.JPEG, 50, out);
            out.close();

            return readSmallBinaryFile(tempFilename);
    }

    public byte[] readSmallBinaryFile(String aFileName) throws IOException {
        BufferedInputStream in = new BufferedInputStream(new FileInputStream(aFileName));
      
        ByteArrayOutputStream bs = new ByteArrayOutputStream();               
        BufferedOutputStream out = new BufferedOutputStream(bs);
        byte[] ioBuf = new byte[100000]; //max ~100kb chunk       
        int bytesRead;
        while ((bytesRead = in.read(ioBuf)) != -1){
            out.write(ioBuf, 0, bytesRead);
        }
        out.close();
        in.close();

        return bs.toByteArray();
    }    

    void typeString(String str){
        byte[] b = str.getBytes();

        for(int i = 0; i < b.length; i++)
            getUiDevice().pressKeyCode(b[i]);                
    }        

    boolean compareByteArrays(byte b1[], byte b2[]){
        if(b1.length != b2.length)
            return false;

        for(int i = 0; i < b2.length; i++)
            if(b1[i] != b2[i])
                return false;

        return true;
    }
    
    public class WorkerRunnable implements Runnable{

        protected Socket clientSocket = null;
        protected MultiThreadedServer srv = null;

        protected final OtpErlangAtom ok = new OtpErlangAtom("ok");
        protected final OtpErlangAtom erlTrue = new OtpErlangAtom("true");
        protected final OtpErlangAtom erlFalse = new OtpErlangAtom("false");       

        public WorkerRunnable(Socket clientSocket, MultiThreadedServer srv) {
            this.clientSocket = clientSocket;

            try{
                clientSocket.setSoTimeout(1000);
            }catch(Exception e){
                System.out.println(e.getMessage());
                e.printStackTrace();                
            }

            this.srv = srv;
        }

        public void run() {
            try {
                InputStream in  = clientSocket.getInputStream();
                OutputStream out = clientSocket.getOutputStream();
                BufferedOutputStream bout = new BufferedOutputStream(out);

                int b;
                byte[] bArr = new byte[65535];

                try {
                    while(true){
                        ByteArrayOutputStream bs = new ByteArrayOutputStream();               
                        BufferedOutputStream bs_out = new BufferedOutputStream(bs);                       

                        try{
                            while((b = in.read(bArr, 0, bArr.length)) != -1){
                                bs_out.write(bArr, 0, b);                            
                            }
                        }catch(java.net.SocketTimeoutException e){}

                        bs_out.close();

                        if(bs.size() > 0)
                            executeCmd(bs.toByteArray(), bs.size(), bout);

                        Thread.sleep(10);
                    }                    
                } catch (IOException e1) {
                    e1.printStackTrace();
                }  

                out.close();
                in.close();                
            } catch (Exception e) {
                //report exception somewhere.
                e.printStackTrace();
            }
        }

        void sendAnswer(BufferedOutputStream bout, OtpErlangTuple t) throws IOException{
            OtpOutputStream oOut = new OtpOutputStream(t);
            bout.write(131); // erlang header, strangely didn't added by OtpOutputStream (see http://stackoverflow.com/questions/15189447/jinterface-to-create-external-erlang-term)
            bout.write(oOut.toByteArray());
            bout.flush();   
        }

        void executeCmd(byte[] cmd, int len, BufferedOutputStream bout) throws IOException{
            OtpInputStream oInp = new OtpInputStream(cmd, 0, len, 0);
            try{
                OtpErlangObject obj = OtpErlangObject.decode(oInp);

                OtpErlangObject content[] = ((OtpErlangTuple)obj).elements();
                int arity = ((OtpErlangTuple)obj).arity();

                OtpErlangAtom tag = ((OtpErlangAtom)content[0]);

                switch(tag.toString()) {
                    case "stop":
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        srv.stop();
                        break;
                    case "ping":
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "click":
                        int x = ((OtpErlangLong)content[1]).intValue();
                        int y = ((OtpErlangLong)content[2]).intValue();
                        getUiDevice().click(x, y);
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "run":
                        String component = ((OtpErlangString)content[1]).stringValue(); //"com.siemens.snc.ilogo/qAndroidDevice.QAndroidDevice";
                        String intent = ((OtpErlangString)content[2]).stringValue(); //"android.intent.action.MAIN";
                        String action = "am start -a " + intent + " -n ";

                        System.out.println(action + component);

                        Runtime.getRuntime().exec(action + component);
                        sendAnswer(bout, new OtpErlangTuple(ok));                        
                        break;
                    case "sleep":
                        sleep(((OtpErlangLong)content[1]).intValue());
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "home":
                        getUiDevice().pressHome();
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "type":
                        typeString(((OtpErlangString)content[1]).stringValue());
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "setcropscreenheight":
                        cropHeight = ((OtpErlangLong)content[1]).intValue();
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "getscreen":
                        sendAnswer(bout, 
                            new OtpErlangTuple(new OtpErlangObject[]
                                                    {ok, 
                                                     new OtpErlangBinary(getScreen())
                                                    }
                                                    ));
                        break;
                    case "getscreenfromcache":
                        sendAnswer(bout, 
                            new OtpErlangTuple(new OtpErlangObject[]{
                                                    ok, 
                                                    new OtpErlangBinary(
                                                        screensCache.get(
                                                            ((OtpErlangString)content[1]).toString())
                                                        )
                                                    }
                                                    ));
                        break;
                    case "uploadscreenoncache":
                        screensCache.put(((OtpErlangString)content[1]).toString(), ((OtpErlangBinary)content[2]).binaryValue());                       
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "putscreenoncache":
                        screensCache.put(((OtpErlangString)content[1]).toString(), getScreen());                       
                        sendAnswer(bout, new OtpErlangTuple(ok));
                        break;
                    case "comparescreenoncache":
                        OtpErlangObject res[] = new OtpErlangObject[2];
                        res[0] = ok;
                        if(compareByteArrays(screensCache.get(((OtpErlangString)content[1]).toString()), getScreen())){
                            res[1] = erlTrue;
                        }else{
                            res[1] = erlFalse;
                        }   
                        sendAnswer(bout, new OtpErlangTuple(res));
                        break;
                    default:
                        sendAnswer(bout, new OtpErlangTuple(new OtpErlangAtom("badarg")));
                        break;
                }

            }catch(OtpErlangDecodeException e){
                sendAnswer(bout, new OtpErlangTuple(new OtpErlangAtom("badterm")));
                e.printStackTrace();
            }catch(OtpErlangRangeException e){
                sendAnswer(bout, new OtpErlangTuple(new OtpErlangAtom("badarg")));
                e.printStackTrace();                
            }
        }

    }


    public class MultiThreadedServer implements Runnable{

        protected int          serverPort   = 4848;
        protected ServerSocket serverSocket = null;
        protected boolean      isStopped    = false;
        protected Thread       runningThread= null;

        public MultiThreadedServer(int port){
            this.serverPort = port;
        }

        public void run(){
            synchronized(this){
                this.runningThread = Thread.currentThread();
            }
            openServerSocket();
            while(! this.isStopped){
                Socket clientSocket = null;
                
                try {
                    clientSocket = this.serverSocket.accept();
                } catch (IOException e) {
                    if(isStopped())
                        return;
                    throw new RuntimeException(
                        "Error accepting client connection", e);
                }
                
                new Thread(
                    new WorkerRunnable(clientSocket, this)).start();
            }
            
            System.out.println("Server Stopped.") ;
        }


        private synchronized boolean isStopped() {
            return this.isStopped;
        }

        public synchronized void stop(){
            this.isStopped = true;
            try {
                this.serverSocket.close();
            } catch (IOException e) {
                throw new RuntimeException("Error closing server", e);
            }
        }

        private void openServerSocket() {
            try {
                this.serverSocket = new ServerSocket(this.serverPort);
            } catch (IOException e) {
                throw new RuntimeException("Cannot open port.", e);
            }
        }
    }

    public void testStart() throws UiObjectNotFoundException, IOException {
        MultiThreadedServer server = new MultiThreadedServer(9000);
        Thread t = new Thread(server);
        t.start();

        try{
            t.join();
        }catch(Exception e){
            e.printStackTrace();            
        }
    }
}
