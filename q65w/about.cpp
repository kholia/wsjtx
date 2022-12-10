#include "about.h"
#include "revision_utils.hpp"
#include "ui_about.h"

CAboutDlg::CAboutDlg(QWidget *parent) :
  QDialog(parent),
  ui(new Ui::CAboutDlg)
{
  ui->setupUi(this);
  ui->labelTxt->setText("<html><h2>" + QString {"Q65W v"
                + QCoreApplication::applicationVersion ()
                + " " + revision ()}.simplified () + "</h2><br />"
    "Q65W is a wideband receiver for the Q65 protocol, <br />"
    "intended primarily for amateur radio EME communication. <br /><br />"
    "Copyright 2001-2023 by Joe Taylor, K1JT.   Additional <br />"
    "acknowledgments are contained in the source code.");
}

CAboutDlg::~CAboutDlg()
{
  delete ui;
}
