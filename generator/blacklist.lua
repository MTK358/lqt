return {
    --'QByteArray', -- converted directly to and from Lua strings
    'QtPrivate',
    'QAlgorithmsPrivate',
    'QtConcurrent',
    'QtSharedPointer',
    'QScopedPointerPodDeleter',
    'QWaitCondition',
    'QThread',
    'QThreadPool',
    'QThreadStorageData',
    'QObject::moveToThread',
    'QObject::thread',
    'QMutex',
    'QMutexLocker',
    'QMutexData',
    'QFuture',
    'QFutureInterfaceBase',
    'QFutureWatcherBase',
    'QSystemSemaphore',
    'QSharedMemory',
    'QState::addTransition',
    'QString::vsprintf',
    'QString::compare',
    'QString::localeAwareCompare',
    'std',

    'QMetaClassInfo::enclosingMetaObject',
    'QMetaProperty::enclosingMetaObject',
    'QMetaEnum::enclosingMetaObject',
    'QMetaMethod::enclosingMetaObject',

    'QProcess',
    'QProcessEnvironment',
    'QFileInfo',
    'QTranslator',
    'QTextBoundaryFinder',
    'QAbstractEventDispatcher',
    'QInternal',
    'QVector<T>::fromStdVector',
    'QResource',
    'QUrl::data_ptr',
    'QPointF::rx', 'QPointF::ry',
    'QObject::findChild',
    'QObject::findChildren',
    'QHashData',
    'QFile::map',
    'QFile::unmap',
    'QAbstractConcatenable',
    'QAbstractFileEngine',
    'QFSFileEngine',
    'QFile::fileEngine',
    'QLibrary',
    'QList<T>::fromStdList',
    'QRectF::getRect',
    'QRectF::getCoords',
    'QLocale::toULongLong',
    'QLocale::toUInt',
    'QLocale::toDouble',
    'QLocale::toUShort',
    'QLocale::toInt',
    'QLocale::toShort',
    'QLocale::toLongLong',
    'QLocale::toFloat',
    'QRect::getRect',
    'QRect::getCoords',
    'QPersistentModelIndex::internalPointer',
    'QLatin1Literal',
    'QMetaMethod::invoke', -- "QGenericArgument" not registered
    'QAbstractItemModel::createIndex',
    'QSizeF::rwidth',
    'QSizeF::rheight',
    'QSize::rwidth',
    'QSize::rheight',
    'QAbstractFileEngineIterator',
    'QByteArray::toULongLong',
    'QByteArray::toUInt',
    'QByteArray::toDouble',
    'QByteArray::toUShort',
    'QByteArray::toInt',
    'QByteArray::toShort',
    'QByteArray::toLongLong',
    'QByteArray::toFloat',
    'QByteArray::toULong',
    'QByteArray::toLong',
    'QByteArray::data_ptr',
    'QBitArray::data_ptr',
    'QTemporaryFile::fileEngine',
    'QMapData',
    'QVariant::QVariant',
    'QVariant::create',
    'QVariant::data',
    'QVariant::toULongLong',
    'QVariant::toUInt',
    'QVariant::toDouble',
    'QVariant::toUShort',
    'QVariant::toInt',
    'QVariant::toShort',
    'QVariant::toLongLong',
    'QVariant::toFloat',
    'QVariant::toULong',
    'QVariant::toLong',
    'QVariant::toReal',
    'QVariant::qvariant_cast_helper',
    'QVariant::value',
    'QVariant::setValue',
    'QVariant::constData',
    'QVariant::PrivateShared',
    'QVariant::Private',
    'QVariant::Handler',
    'QVariant::data_ptr',
    'QVariant::fromValue',
    'QSignalMapper::mapping',
    'QSignalMapper::setMapping',
    'QSignalMapper::mapped',
    'QString::toWCharArray',
    'QString::toShort',
    'QString::toStdString',
    'QString::toStdWString',
    'QString::setUtf16',
    'QString::toULongLong',
    'QString::toUInt',
    'QString::toDouble',
    'QString::toUShort',
    'QString::toInt',
    'QString::toShort',
    'QString::toLongLong',
    'QString::toFloat',
    'QString::toULong',
    'QString::toLong',
    'QString::toReal',
    'QString::data_ptr',
    'QString::utf16',
    'QString::fromUcs4',
    'QString::fromStdWString',
    'QString::fromStdString',
    'QString::fromUtf16',
    'QString::fromWCharArray',
    'QListData',
    'QModelIndex::internalPointer',
    'QCoreApplicationPrivate',
    'QCoreApplication::compressEvent',
    'QCoreApplication::filterEvent',
    'QCoreApplication::removeTranslator',
    'QCoreApplication::installTranslator',
    'QCoreApplication::argv',
    'QDate::weekNumber',
    'QDate::getDate',
    'QDate::julianToGregorian',
    'QDataStream::readBytes',
    'QDirIterator::fileInfo',
    'QAnimationDriver::QAnimationDriver',
    'QMetaType::destroy',
    'QMetaType::load',
    'QMetaType::construct',
    'QMetaType::save',
    'QForeachContainerBase',
    'QObjectData',
    'QObjectPrivate',
    'QTextCodec::ConverterState',
    'QObjectCleanupHandler',
    'QXmlStreamStringRef::string',
    'QAnimationDriver',
    'QEventLoop::QEventLoop',
    'QFileSystemWatcher',
    'QLatin1Char::toLatin1',
    'QStringRef::compare',
    'QStringRef::localeAwareCompare',
    'QModelIndex::model',
    'QPersistentModelIndex::model',
    'QMetaProperty',
    'QHash<Key,T>::iterator<Key,T>',
    'QHash<Key,T>::const_iterator<Key,T>',
    'QHashNode<Key,T>',
    'QVectorTypedData<T>',
    'QVector<T>::toStdVector',
    'QVector<T>::data',
    'QVector<T>::begin',
    'QVector<T>::end',
    'QVector<T>::erase',
    'QVector<T>::constBegin',
    'QVector<T>::constEnd',
    'QVector<T>::constData',
    'QVector<T>::insert',
    'QVector<T>::last',
    'QVector<T>::back',
    'QVector<T>::front',
    'QVector<T>::first',
    'QVector<QFileInfo>',
    'QFile::readLineData',
    'QFile::readData',
    'QList<T>::toStdList',
    'QList<T>::data',
    'QList<T>::begin',
    'QList<T>::end',
    'QList<T>::erase',
    'QList<T>::constBegin',
    'QList<T>::constEnd',
    'QList<T>::constData',
    'QList<T>::insert',
    'QList<T>::last',
    'QList<T>::back',
    'QList<T>::front',
    'QList<T>::first',
    'QBuffer::readData',
    'QFlags<Enum>::QFlags',
    'QLibrary',
    'QByteArray::data',
    'QByteArray::begin',
    'QByteArray::end',
    'QByteArray::erase',
    'QByteArray::constBegin',
    'QByteArray::constEnd',
    --'QByteArray::constData',
    'QByteArray::insert',
    'QByteArray::last',
    'QByteArray::back',
    'QByteArray::front',
    'QByteArray::first',
    'QSet<T>::contains',
    'QSet<T>::constFind',
    'QSet<T>::find',
    'QSet<T>::data',
    'QSet<T>::begin',
    'QSet<T>::end',
    'QSet<T>::erase',
    'QSet<T>::constBegin',
    'QSet<T>::constEnd',
    'QSet<T>::constData',
    'QSet<T>::insert',
    'QSet<T>::last',
    'QSet<T>::back',
    'QSet<T>::front',
    'QSet<T>::first',
    'QList<QFileInfo>',
    'std::map<Key,T>',
    'QMap<Key,T>::contains',
    'QMap<Key,T>::constFind',
    'QMap<Key,T>::find',
    'QMap<Key,T>::data',
    'QMap<Key,T>::begin',
    'QMap<Key,T>::end',
    'QMap<Key,T>::erase',
    'QMap<Key,T>::constBegin',
    'QMap<Key,T>::constEnd',
    'QMap<Key,T>::constData',
    'QMap<Key,T>::insert',
    'QMap<Key,T>::last',
    'QMap<Key,T>::back',
    'QMap<Key,T>::front',
    'QMap<Key,T>::first',
    'QMap<Key,T>::insertMulti',
    'QMap<Key,T>::upperBound',
    'QMap<Key,T>::lowerBound',
    'QIODevice::readLineData',
    'QIODevice::readData',
    'QIODevice::readLine',
    'QIODevice::getChar',
    'QIODevice::peek',
    'QIODevice::read',
    'QPluginLoader',
    'QDataStream',
    'QEasingCurve::customType',
    'QEasingCurve::setCustomType',
    'QEasingCurve::EasingFunction',
    'QVariant::canConvert',
    'QVariant::qRegisterGuiVariant',
    'QVariant::qUnregisterGuiVariant',
    'QTextCodec',
    'QHash<Key,T>',
    'QHash<int,QByteArray>',
    'QHash<QString,QVariant>',
    'QSet<T>::remove',
    'QSet<T>::intersect',
    'QList<T>::toSet',
    'QVector<T>::toSet',
    'QSet<T>',
    'QSet<QAbstractState*>',
    'Qt::Modifier',
    'QObject::connect', -- custom reimplementation
}
