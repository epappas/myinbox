package com.evalonlabs.myinbox.smtp;

import org.subethamail.smtp.server.SMTPServer;

public class BasicSMTPServer {
    public static void main(String[] args) {
        com.evalonlabs.myinbox.smtp.MyMessageHandlerFactory myFactory = new com.evalonlabs.myinbox.smtp.MyMessageHandlerFactory() ;
        SMTPServer smtpServer = new SMTPServer(myFactory);
        smtpServer.setPort(25000);
        smtpServer.start();
    }
}