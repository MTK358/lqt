
local listcontainersubs = {
    'QString',
    'QAbstractAnimation*', 
    'QVariant',
    'QXmlStreamAttribute',
    'QXmlStreamNamespaceDeclaration',
    'QXmlStreamEntityDeclaration',
    'QXmlStreamNotationDeclaration',
    'QAbstractTransition*',
    'uint',
    'QPair<QByteArray,QByteArray>',
    'QByteArray',
    'QPair<QString,QString>',
    'QObject*',
    'Qt::DayOfWeek',
    'QLocale',
    'QLocale::Country',
    'QModelIndex',
    'QAbstractState*',
    --'QFileInfo',
    'int',
    'QVariantAnimation::KeyValue',
    'QUrl',
}

return {
    -- usage:
    -- ['Example<T,U>'] = {'int,float', 'std::string,std::string'},
    
    ['QPair<T1,T2>'] = {
        'QByteArray,QByteArray',
        'QString,QString',
        'qreal,QVariant',
    },
    ['QList<T>'] = listcontainersubs,
    ['QVector<T>'] = listcontainersubs,
    --['QSet<T>'] = listcontainersubs,
    ['QHash<Key,T>'] = {
        --'int,QByteArray',
        --'QString,QVariant',
    },
    ['QMap<Key,T>'] = {
        'int,QVariant',
        'QString,QVariant',
    },
    ['QFlags<Enum>'] = {
        'QIODevice::OpenModeFlag',
        'QUrl::FormattingOption',
        'QFile::FileHandleFlag',
        'QFile::Permission',
        --'QLibrary::LoadHint',
        'QLocale::NumberOption',
        'Qt::ItemFlag',
        'Qt::MatchFlag',
        'Qt::DropAction',
        --'QTextCodec::ConversionFlag',
        'QEventLoop::ProcessEventsFlag',
        'QDir::SortFlag',
        'QDir::Filter',
        'QTextStream::NumberFlag',
        'QString::SectionFlag',
        'QDirIterator::IteratorFlag',
    },
}
