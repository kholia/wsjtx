#ifndef WSJTX_FILEDOWNLOAD_H
#define WSJTX_FILEDOWNLOAD_H

#include <QObject>
#include <QString>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>
#include <QTemporaryFile>

class FileDownload : public QObject {
    Q_OBJECT

public:
    explicit FileDownload();
    ~FileDownload();

    void configure(const QString& source_url, const QString& destination_filename);

private:
    QNetworkAccessManager *manager_;
    QString source_url_;
    QString destination_filename_;
    QNetworkReply *reply_;
    QNetworkRequest *request_;
    QTemporaryFile *tmpfile_;
    QDir *tmpdir_;
signals:
            void complete(QString filename);

public slots:
    void download();
    void store();
    void downloadComplete(QNetworkReply* data);
    void downloadProgress(qint64 recieved, qint64 total);
    void errorOccurred(QNetworkReply::NetworkError code);
    void replyComplete();
};

#endif //WSJTX_FILEDOWNLOAD_H
