import { useState, useEffect } from 'react'
import './ResultsDisplay.css'

interface ResultsDisplayProps {
  result: {
    status: 'processing' | 'complete' | 'error'
    explanation?: string
    architecture?: string
    artifacts?: {
      lambda: boolean
      apiGateway: boolean
      cdk: boolean
    }
    error?: string
  }
  analysisId: string | null
}

function ResultsDisplay ({ result, analysisId }: ResultsDisplayProps) {
  const [activeTab, setActiveTab] = useState<
    'explanation' | 'architecture' | 'code'
  >('explanation')

  useEffect(() => {
    if (activeTab === 'architecture' && result.architecture) {
      // Dynamically import mermaid to avoid SSR issues
      import('mermaid').then(mermaid => {
        mermaid.default.initialize({ startOnLoad: true, theme: 'default' })
        mermaid.default.contentLoaded()
      })
    }
  }, [activeTab, result.architecture])

  if (result.status === 'error') {
    return (
      <div className='results-display error'>
        <h3>❌ Analysis Error</h3>
        <p>{result.error}</p>
      </div>
    )
  }

  if (result.status !== 'complete') {
    return null
  }

  const handleDownload = async (artifact: string) => {
    if (!analysisId) return

    try {
      const response = await fetch(`/api/download/${analysisId}/${artifact}`)
      if (!response.ok) throw new Error('Download failed')

      const blob = await response.blob()
      const url = window.URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `${artifact}.txt`
      document.body.appendChild(a)
      a.click()
      window.URL.revokeObjectURL(url)
      document.body.removeChild(a)
    } catch (error) {
      alert('Failed to download artifact')
    }
  }

  return (
    <div className='results-display'>
      <div className='tabs'>
        <button
          className={`tab ${activeTab === 'explanation' ? 'active' : ''}`}
          onClick={() => setActiveTab('explanation')}
        >
          📝 Explanation
        </button>
        <button
          className={`tab ${activeTab === 'architecture' ? 'active' : ''}`}
          onClick={() => setActiveTab('architecture')}
        >
          🏗️ Architecture
        </button>
        <button
          className={`tab ${activeTab === 'code' ? 'active' : ''}`}
          onClick={() => setActiveTab('code')}
        >
          💻 Generated Code
        </button>
      </div>

      <div className='tab-content'>
        {activeTab === 'explanation' && (
          <div className='explanation'>
            <h3>Business Logic Explanation</h3>
            <div className='explanation-text'>{result.explanation}</div>
          </div>
        )}

        {activeTab === 'architecture' && (
          <div className='architecture'>
            <h3>AWS Architecture</h3>
            <div className='mermaid'>{result.architecture}</div>
          </div>
        )}

        {activeTab === 'code' && (
          <div className='code-artifacts'>
            <h3>Generated AWS Code</h3>
            <div className='artifacts-list'>
              {result.artifacts?.lambda && (
                <div className='artifact-item'>
                  <span>Lambda Function (TypeScript)</span>
                  <button onClick={() => handleDownload('lambda')}>
                    Download
                  </button>
                </div>
              )}
              {result.artifacts?.apiGateway && (
                <div className='artifact-item'>
                  <span>API Gateway Configuration</span>
                  <button onClick={() => handleDownload('apiGateway')}>
                    Download
                  </button>
                </div>
              )}
              {result.artifacts?.cdk && (
                <div className='artifact-item'>
                  <span>AWS CDK Stack</span>
                  <button onClick={() => handleDownload('cdk')}>
                    Download
                  </button>
                </div>
              )}
              <div className='artifact-item highlight'>
                <span>All Artifacts (Bundle)</span>
                <button onClick={() => handleDownload('all')}>
                  Download All
                </button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

export default ResultsDisplay
