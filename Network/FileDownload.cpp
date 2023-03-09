
#include "FileDownload.hpp"
#include <QCoreApplication>
#include <QUrl>
#include <QNetworkRequest>
#include <QFileInfo>
#include <QDir>
#include <QTemporaryFile>
#include "qt_helpers.hpp"
#include "Logger.hpp"

FileDownload::FileDownload() : QObject(nullptr)
{

}

FileDownload::~FileDownload()
{
}

void FileDownload::errorOccurred(QNetworkReply::NetworkError code)
{
  LOG_INFO(QString{"DOWNLOAD: errorOccurred %1 -> %2"}.arg(code).arg(reply_->errorString()));
  //LOG_INFO(QString{ "DOWNLOAD: server returned %1"}.arg(reply_->))
}

void FileDownload::configure(const QString &source_url, const QString &destination_path)
{
  source_url_ = source_url;
  destination_filename_ = destination_path;
}

void FileDownload::store()
{
  if (tmpfile_->isOpen())
    tmpfile_->write (reply_->read (reply_->bytesAvailable ()));
  else
    LOG_INFO(QString{ "DOWNLOAD: tmpfile is not open"});
}

void FileDownload::replyComplete()
{
  auto is_error = reply_->error ();
  LOG_INFO(QString{"DOWNLOAD: reply complete %1"}.arg(is_error));
  if (reply_ && reply_->isFinished ())
  {
    reply_->deleteLater ();
  }
}

void FileDownload::downloadComplete(QNetworkReply *data)
{
  // make a temp file in the same place as the file we're downloading. Needs to be on the same
  // filesystem as where we eventually want to 'mv' it.

  QUrl r = request_->url();
  LOG_INFO(QString{"DOWNLOAD: finished download %1 -> %2 (%3)"}.arg(source_url_).arg(destination_filename_).arg(r.url()));

  LOG_INFO(QString{ "DOWNLOAD: tempfile path is %1"}.arg(tmpfile_->fileName()));

  tmpfile_->close();

  LOG_INFO(QString{"DOWNLOAD: moving file to %2"}.arg(destination_filename_));

  LOG_INFO("Request Headers:");
  Q_FOREACH (const QByteArray& hdr, request_->rawHeaderList()) {
      LOG_INFO(QString{ "%1 -> %2"}.arg(QString(hdr)).arg(QString(request_->rawHeader(hdr))));
    }

  LOG_INFO("Response Headers:");
  Q_FOREACH (const QByteArray& hdr, reply_->rawHeaderList()) {
      LOG_INFO(QString{ "%1 -> %2"}.arg(QString(hdr)).arg(QString(reply_->rawHeader(hdr))));
  }
  // move the file to the destination
  tmpdir_->remove(destination_filename_+".old"); // get rid of previous version
  tmpdir_->rename(destination_filename_, destination_filename_+".old");
  tmpdir_->rename(tmpfile_->fileName(), destination_filename_);
  emit complete(destination_filename_);
  data->deleteLater();
}

void FileDownload::download()
{
  //QUrl url = QUrl(source_url_);

  manager_ = new QNetworkAccessManager(this);

  // request_ = new QNetworkRequest("https://www.country-files.com/bigcty/cty.dat");
  request_ = new QNetworkRequest(QUrl(source_url_));

  LOG_INFO(QString{"DOWNLOAD: starting download %1 -> %2"}.arg(source_url_).arg(destination_filename_));

  request_->setAttribute(QNetworkRequest::FollowRedirectsAttribute, true);
  //request_->setHeader( QNetworkRequest::ContentTypeHeader, "some/type" );
  request_->setRawHeader("Accept", "*/*");
  request_->setRawHeader ("User-Agent", "WSJT-X CTY Downloader");

  reply_ = manager_->get(*request_);

  reply_->setReadBufferSize(0);

  int http_code = reply_->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();

  QObject::connect(manager_, &QNetworkAccessManager::finished, this, &FileDownload::downloadComplete);
  QObject::connect(reply_, &QNetworkReply::downloadProgress, this, &FileDownload::downloadProgress);
  QObject::connect(reply_, &QNetworkReply::finished, this,&FileDownload::replyComplete);
  QObject::connect(reply_, &QNetworkReply::errorOccurred,this,&FileDownload::errorOccurred);
  QObject::connect (reply_, &QNetworkReply::finished, this, &FileDownload::replyComplete);
  QObject::connect (reply_, &QNetworkReply::readyRead, this, &FileDownload::store);

  QFileInfo tmpfi(destination_filename_);
  QString const tmpfile_path = tmpfi.absolutePath();
  tmpdir_ = new QDir(tmpfile_path);
  tmpfile_ = new QTemporaryFile(tmpfile_path+"/big.cty.XXXXXX");
  if (!tmpfile_->open())
  {
    LOG_INFO(QString{"DOWNLOAD: Unable to open the temporary file based on %1"}.arg(tmpfile_path));
    return;
  }
  LOG_INFO(QString{"DOWNLOAD: let's go %1"}.arg(http_code));
}

void FileDownload::downloadProgress(qint64 received, qint64 total)
{
  LOG_INFO(QString{"DOWNLOAD: Progress %1 from %2, total %3, so far %4"}.arg(destination_filename_).arg(source_url_).arg(total).arg(received));
  //qDebug() << received << total;
}
