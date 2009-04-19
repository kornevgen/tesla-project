package ru.teslaprj;

public interface Cache
{
//    public static final long INVALID_TAG = 0xffffffffffffffffL;

    /** ассоциативность кэша == размер "сет"а */
    int getSectionNumber();
    
    int getAddressBitLength();
    
    int getTagBitLength();
    
    int getSetNumberBitLength();

//    int getRowNumber();
//    
//    int getRowSize();
//    
//    long getTag(long physicalAddress);
//
//    int getRow(long physicalAddress);
//    
//    int getPosition(long physicalAddress);
//
//    long getPhysicalAddress(long tag, int row, int pos);
//
//    boolean contains(long physicalAddress);
//    
//    byte[] read(long physicalAddress);
//    
//    void write(long physicalAddress, byte data[]);
//
    long getTag(int section, int set);
//    
//    void setTag(int section, int row, long tag);
//    
//    byte[] getData(int section, int row);
//    
//    void setData(int section, int row, byte data[]);
}
