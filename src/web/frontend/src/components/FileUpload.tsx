import { useRef, useState } from 'react'
import './FileUpload.css'

interface FileUploadProps {
  onFileUpload: (content: string) => void
}

function FileUpload ({ onFileUpload }: FileUploadProps) {
  const [isDragging, setIsDragging] = useState(false)
  const fileInputRef = useRef<HTMLInputElement>(null)

  const handleDragOver = (e: React.DragEvent) => {
    e.preventDefault()
    setIsDragging(true)
  }

  const handleDragLeave = () => {
    setIsDragging(false)
  }

  const handleDrop = (e: React.DragEvent) => {
    e.preventDefault()
    setIsDragging(false)

    const files = e.dataTransfer.files
    if (files.length > 0) {
      readFile(files[0])
    }
  }

  const handleFileSelect = (e: React.ChangeEvent<HTMLInputElement>) => {
    const files = e.target.files
    if (files && files.length > 0) {
      readFile(files[0])
    }
  }

  const readFile = (file: File) => {
    const reader = new FileReader()
    reader.onload = e => {
      const content = e.target?.result as string
      onFileUpload(content)
    }
    reader.readAsText(file)
  }

  return (
    <div
      className={`file-upload ${isDragging ? 'dragging' : ''}`}
      onDragOver={handleDragOver}
      onDragLeave={handleDragLeave}
      onDrop={handleDrop}
      onClick={() => fileInputRef.current?.click()}
    >
      <input
        ref={fileInputRef}
        type='file'
        accept='.cbl,.cob,.cobol'
        onChange={handleFileSelect}
        style={{ display: 'none' }}
      />
      <span>📁 Upload COBOL File</span>
    </div>
  )
}

export default FileUpload
