document.addEventListener('DOMContentLoaded', function () {
  console.log("JavaScript cargado correctamente");

  let configPanel = document.querySelector('[id*="theme_view"]');
  let dataVizPanel = document.querySelector('.panel:last-of-type');
  let lastTop, lastLeft, lastWidth, lastHeight, originalDataVizWidth;


  if (configPanel && dataVizPanel) {
    setTimeout(() => {
      let rect = configPanel.getBoundingClientRect();
      savePanelPosition(configPanel, rect);

      originalDataVizWidth = dataVizPanel.offsetWidth;
      configPanel.style.display = "none"; // Ocultar después de guardar su estado
    }, 500);
  }

  const observer = new MutationObserver(() => {
    let openConfigBtn = document.querySelector('#open_config');
    configPanel = document.querySelector('[id*="theme_view"]');
    //dataVizPanel = document.querySelector('#data-viz');

    if (openConfigBtn && configPanel && dataVizPanel) {
      observer.disconnect();
      initConfigPanel(openConfigBtn, configPanel, dataVizPanel);
    }
  });

  observer.observe(document.body, { childList: true, subtree: true });

  function savePanelPosition(panel, rect) {
    lastTop = rect.top + "px";
    lastLeft = rect.left + "px";
    lastWidth = panel.offsetWidth + "px";
    lastHeight = panel.offsetHeight + "px";
  }

  function initConfigPanel(openConfigBtn, configPanel, dataVizPanel) {
    let isMoved = false, isResized = false;

    openConfigBtn.addEventListener('click', function () {
      if (configPanel.style.display === 'none') {
        showConfigPanel(configPanel);
      } else {
        hideConfigPanel(configPanel);
      }
    });

    let closeButton = configPanel.querySelector('.panel-header-dismiss');
    if (closeButton) {
      closeButton.addEventListener('click', function () {
        hideConfigPanel(configPanel);
      });
    }

    makePanelDraggable(configPanel);
    makePanelResizable(configPanel);
  }

  function showConfigPanel(configPanel) {
    configPanel.style.display = 'block';
    configPanel.style.top = lastTop;
    configPanel.style.left = lastLeft;
    configPanel.style.width = lastWidth;
    configPanel.style.height = lastHeight;

    //adjustDataPanel();
    console.log("Panel mostrado y ajustando tamaño del panel de datos.");
  }

  function hideConfigPanel(configPanel) {
    savePanelPosition(configPanel, configPanel.getBoundingClientRect());
    configPanel.style.display = 'none';
    //adjustDataPanel();
    console.log("Panel ocultado y restaurando tamaño del panel de datos.");
  }

  function makePanelDraggable(configPanel) {
    let isDragging = false, offsetX, offsetY;
    let header = configPanel.querySelector('.panel-header');

    if (header) {
      header.style.cursor = "grab";

      header.addEventListener('mousedown', function (e) {
        configPanel.style.position = "absolute";
        isDragging = true;
        isMoved = true;
        let rect = configPanel.getBoundingClientRect();
        offsetX = e.clientX - rect.left;
        offsetY = e.clientY - rect.top;
        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
      });
    }

    function onMouseMove(e) {
      if (isDragging) {
        configPanel.style.left = `${e.clientX - offsetX}px`;
        configPanel.style.top = `${e.clientY - offsetY}px`;
      }
    }

    function onMouseUp() {
      isDragging = false;
      document.removeEventListener('mousemove', onMouseMove);
      document.removeEventListener('mouseup', onMouseUp);
    }
  }

  function makePanelResizable(configPanel) {
    let resizeHandle = document.createElement('div');
    resizeHandle.classList.add("resize-handle");
    configPanel.appendChild(resizeHandle);

    let isResizing = false;

    resizeHandle.addEventListener('mousedown', function (e) {
      isResizing = true;
      e.preventDefault();
      document.addEventListener('mousemove', onResize);
      document.addEventListener('mouseup', onResizeEnd);
    });



    function onResize(e) {
      if (isResizing) {
        let newWidth = e.clientX - configPanel.getBoundingClientRect().left;
        let newHeight = e.clientY - configPanel.getBoundingClientRect().top;

        if (newWidth > 200) configPanel.style.width = `${newWidth}px`;
        if (newHeight > 150) configPanel.style.height = `${newHeight}px`;
      }
    }

    function onResizeEnd() {
      isResizing = false;
      document.removeEventListener('mousemove', onResize);
      document.removeEventListener('mouseup', onResizeEnd);
    }
  }
});
